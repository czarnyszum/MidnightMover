{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Ctx where

import Control.Exception
import Control.Monad
import Control.Monad.Except 
import Control.Monad.State
import Control.Lens hiding (element)

-- import Data.ByteString.Lazy (ByteString)
-- import qualified Data.Text as T
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-- import Data.Sequence (Seq)
import Data.Time

import GHC.Generics (Generic)
--import qualified Data.ByteString.Char8 as B

import System.FilePath ((</>))

import Network.Wreq hiding (put, statusCode, get) 
import Network.HTTP.Client (Manager, CookieJar, cookieJar, createCookieJar, requestHeaders, parseRequest, httpLbs, updateCookieJar)
import Network.HTTP.Types.Status (statusCode)

import Text.HTML.DOM
import Text.XML.Cursor

-- import Text.HTML.TagSoup
--import Text.Blaze.Html (Html)
--import qualified Text.Blaze.Html.Renderer.Text as Blaze
--import qualified Text.Blaze.Html5 as H
--import qualified Text.Blaze.Html5.Attributes as A

-- import Network.TLS.Extra.Cipher (ciphersuite_default)
-- import qualified Data.X509 as X509

import Parse
import Post
import TlsManager 


data Output = OutputFile | OutputBunker String String deriving (Eq, Ord, Show, Generic)
instance FromJSON Output

data User = User
  { _userLogin    :: String
  , _userPassword :: String
  , _userThreads  :: [String]
  , _userFilter   :: [String]
  , _userOutput   :: Output
  } deriving (Show)
makeLenses ''User

-- экземпляр FromJSON для User
instance FromJSON User where
  parseJSON = withObject "User" $ \o -> do
    login      <- o .: "login"
    password   <- o .: "password"
    threads    <- o .: "threads"
    output     <- o .: "output"
    users      <- o .: "userFilter"
    return $ User login password threads users output

-- Читает User из JSON-файла basePath/user.txt
readUser :: String -> IO (Maybe User)
readUser basePath = do
  let path = basePath </> "user.txt"
  c <- BL.readFile path
  case eitherDecode c of
    Left _err   -> return Nothing
    Right user  -> return (Just user)

loginAddr :: String
loginAddr = "https://simsmix.ru/forum/login/login"          

data Ctx = Ctx
  { _ctxCookieJar :: CookieJar  -- cookie jar after login
  , _ctxBaseUrl   :: String     -- base url of forum 
  , _ctxUser      :: Maybe User -- maybe user info
  , _ctxManager   :: Manager    -- tls manager
  }
makeLenses ''Ctx

data ErrorKind
  = NetworkError String        -- проблемы с сетью / HTTP
  | LoginFailed String         -- логин не удался (не 2xx, редирект на /login, и т.п.)
  | ProtoError String          -- неожиданный ответ
  | TokenError
  deriving (Show, Eq)

emptyCtx :: IO Ctx
emptyCtx =
  do
    mng <- mkTlsManager
    let
      ctx = Ctx
        { _ctxCookieJar = createCookieJar []
        , _ctxBaseUrl   = "https://simsmix.ru/forum"
        , _ctxUser      = Nothing
        , _ctxManager   = mng 
        }
    return ctx

extractXFToken :: (MonadError ErrorKind m) => Cursor -> m Text
extractXFToken c =
  case c $// element "input" >=> attributeIs "name" "_xfToken" >=> attribute "value" of
    (t : _) -> return t
    _ -> throwError TokenError
    
  
getXFToken :: (MonadError ErrorKind m, MonadState Ctx m, MonadIO m) => User -> m Text
getXFToken u =
  do
    ctx <- get
    let
      cks = _ctxCookieJar ctx
      mgr = _ctxManager ctx
      rqHd = [("User-Agent", "MidnightMover/0.0")]
  
    initReq <- liftIO $ parseRequest loginAddr     
    let req = initReq  { requestHeaders = rqHd }
    response <- liftIO $ httpLbs req mgr
    let
      cj = response ^. responseCookieJar
      body = view responseBody response
      doc  = parseLBS body
      cursor = fromDocument doc
    put $ ctx & ctxCookieJar .~ cj
    extractXFToken cursor

login :: (MonadError ErrorKind m, MonadState Ctx m, MonadIO m) => User -> m ()
login user = do

  token <- getXFToken user
  liftIO $ print token
    
  ctx <- get
  let cj0   = ctx ^. ctxCookieJar
      mgr   = ctx ^. ctxManager
      opts0 = defaults
                & cookies .~ Just cj0
                & manager .~ Right mgr

      formParams =
        [ "login"        := (user ^. userLogin)
        , "register"     := ("0" :: String)
        , "password"     := (user ^. userPassword)
        , "cookie_check" := ("1" :: String)
        , "_xfToken"     := (T.unpack token)
        , "redirect"     := ("/forum/" :: String)
        ]

  r <- liftIO (postWith opts0 loginAddr formParams)
       `catchLift` NetworkError

  let status = r ^. responseStatus . to statusCode
      cj1    = r ^. responseCookieJar

  liftIO $ BL.writeFile "response" (r ^. responseBody)  -- (T.decodeUtf8 ($ r ^. responseBody))
 
  if status >= 200 && status < 300
    then do
      put $ ctx & ctxCookieJar .~ cj1
                & ctxUser      .~ Just user
      liftIO $ putStrLn "Login succeeded."
 {-
      let
        body = view responseBody r
        doc  = parseLBS body
        cursor = fromDocument doc
      token' <- extractXFToken cursor
      
      liftIO $ putStrLn $ "Token: " ++ (T.unpack token')
-}
    else do
      liftIO $ putStrLn $ "Login failed, HTTP status: " ++ show status
      throwError $ LoginFailed ("HTTP status " ++ show status)

-- вспомогалка: поднимаем IO-ошибку в MonadError
catchLift :: (MonadError e m, MonadIO m) => IO a -> (String -> e) -> m a
catchLift action wrap = do
  r <- liftIO $ (Right <$> action) `catch` (\(e :: IOError) -> return (Left (show e)))
  case r of
    Left err  -> throwError (wrap err)
    Right val -> return val
  
getPageCursor
  :: (MonadError ErrorKind m, MonadState Ctx m, MonadIO m)
  => String  -- ^ Адрес страницы (относительный или полный)
  -> m Cursor
getPageCursor addr = do
  ctx <- get
  let
    fullUrl = addr
    cks = _ctxCookieJar ctx
    mgr = _ctxManager ctx
    rqHd = [("User-Agent", "MidnightMover/0.0")]
  
  initReq <- liftIO $ parseRequest fullUrl
  let req = initReq
             { cookieJar = Just cks
             , requestHeaders = rqHd
             }

  liftIO $ print (cookieJar req)

  response <- liftIO $ httpLbs req mgr
  
  now <- liftIO getCurrentTime
  let (newJar, _) = updateCookieJar response req now cks

  let status = statusCode (view responseStatus response)
  
  unless (status >= 200 && status < 300) $
    throwError $ NetworkError ("HTTP error: " ++ show status)

  put $ ctx & ctxCookieJar .~ newJar

  let
    body = view responseBody response
    doc  = parseLBS body
  return (fromDocument doc)

processMessage :: (MonadError ErrorKind m, MonadState Ctx m, MonadIO m) => User -> Message -> m ()
processMessage user (p, u, c) =
  do
    let
      postname = T.unpack p
      username = T.unpack u
      filename = postname ++ "-" ++  username      
    if username `elem` (view userFilter user)
    then
      do
        let
          post = extractPost c
          (bs, bi, bd) = isValidPost post
        if bd || (bs && bi)
        then
          do 
            savePost filename post
            liftIO $ putStrLn $ filename ++ " выводим"
        else
          liftIO $ putStrLn $ filename ++ " пропускаем, это коментарий"
    else
      liftIO $ putStrLn $ filename ++ " пропускаем, не тот автор"


processPage :: (MonadError ErrorKind m, MonadState Ctx m, MonadIO m) => User -> String -> m ()
processPage user addr =
  do
    liftIO $ putStrLn $ "Loading: " ++ addr
    cursor <- getPageCursor addr
    let
      messages = extractMessages cursor
    mapM_ (processMessage user) messages
    
move :: (MonadError ErrorKind m, MonadState Ctx m, MonadIO m) => User -> m ()
move user =
  do
    let
      threads = view userThreads user
      thread0 = threads !! 0
    login user
    cursor <- getPageCursor thread0
    let
      maybePageNumber = exractPageNumber cursor    
    case maybePageNumber of
     Just n ->
       do
        let
          pager y x = y ++ "page-" ++ (show x)
          pages = thread0 : map (pager thread0) [141, 502, 503, 505, 507, 518, 522, 532] -- ([2 .. 10] ++ [122, 168, 248] ++ [250 .. 255]) -- 2 .. n
        liftIO . putStrLn $ "Total: " ++ (show n)
        mapM_ (processPage user) pages
     Nothing -> liftIO . putStrLn $ "Не нашел счетчик страниц"
