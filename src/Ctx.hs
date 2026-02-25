{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Ctx where

import Control.Exception
import Control.Monad.Except 
import Control.Monad.State
import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import System.FilePath ((</>))
import Network.Wreq hiding (put, statusCode, get) 
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client (Manager, CookieJar, createCookieJar)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Client.TLS
import Network.TLS (defaultSupported)
import Network.TLS.Extra.Cipher (ciphersuite_default)
import qualified Data.X509 as X509

import TlsManager 

data User = User
  { _userLogin    :: String
  , _userPassword :: String
  } deriving (Show)
makeLenses ''User

-- экземпляр FromJSON для User
instance FromJSON User where
  parseJSON = withObject "User" $ \o -> do
    login    <- o .: "login"
    password <- o .: "password"   -- или "passward", смотри ниже
    return $ User login password

-- Читает User из JSON-файла basePath/user.txt
readUser :: String -> IO (Maybe User)
readUser basePath = do
  let path = basePath </> "user.txt"
  content <- BL.readFile path
  case eitherDecode content of
    Left _err   -> return Nothing
    Right user  -> return (Just user)

loginAddr :: String
loginAddr = "https://simsmix.ru/forum/login/login"          

data Ctx = Ctx
  { _ctxCookieJar :: CookieJar
  , _ctxBaseUrl   :: String
  , _ctxUser      :: Maybe User
  , _ctxManager   :: Manager
  }
makeLenses ''Ctx

data ErrorKind
  = NetworkError String        -- проблемы с сетью / HTTP
  | LoginFailed String         -- логин не удался (не 2xx, редирект на /login, и т.п.)
  | ProtoError String          -- неожиданный ответ
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

login :: (MonadError ErrorKind m, MonadState Ctx m, MonadIO m) => User -> m ()
login user = do
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
        , "_xfToken"     := (""  :: String)
        , "redirect"     := (ctx ^. ctxBaseUrl)
        ]

  r <- liftIO (postWith opts0 loginAddr formParams)
       `catchLift` NetworkError

  let status = r ^. responseStatus . to statusCode
      cj1    = r ^. responseCookieJar

  if status >= 200 && status < 300
    then do
      put $ ctx & ctxCookieJar .~ cj1
                & ctxUser      .~ Just user
      liftIO $ putStrLn "Login succeeded."
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
  
