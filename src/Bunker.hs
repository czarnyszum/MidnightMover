{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bunker (loginBunker) where

import Control.Exception (bracket, try, SomeException)
import Control.Lens hiding (element)
import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import Data.ByteString.Char8 (ByteString, pack, unpack)
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as BL

import Data.Char (isSpace, toLower, ord, chr, isAlphaNum)
import Data.List
import Data.Maybe (catMaybes, fromJust)
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Text (Text)
import qualified Data.Text as T

import Network.HTTP.Client
import Network.HTTP.Types.Header (hReferer, hContentType, HeaderName)
import Network.HTTP.Types.Status (statusIsSuccessful)
-- import Network.HTTP.Types.URI (urlEncode)

import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)


import Ctx
import Token

-- | Constants
userAgent :: B.ByteString
userAgent = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36" -- "MidnightMover/0.0"

indexUrl :: String
indexUrl = "https://gamestories.clanboard.ru/index.php"

-- | Execute an HTTP request, update the cookie jar in the state,
--   and return the response.  Throws 'ErrorKind' on failure.
httpRequest
    :: (MonadError ErrorKind m, MonadState Ctx m, MonadIO m)
    => Request         -- ^ base request (method, url, etc.)
    -> [(HeaderName, B.ByteString)] -- ^ extra headers
    -> Maybe RequestBody   -- ^ optional request body
    -> m (Response BL.ByteString)
httpRequest baseReq extraHeaders mBody = do
    ctx <- get
    let
        mgr = _ctxManager ctx
        cj  = _ctxCookieJar ctx

        -- Add headers and cookie jar to the request
        reqWithHeaders = baseReq
            {  
              requestHeaders = extraHeadersStd ++ extraHeaders
            , cookieJar = Just cj
            }
        extraHeadersStd = [ ("User-Agent", userAgent), ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8") ]

        reqFinal = case mBody of
            Nothing -> reqWithHeaders
            Just body -> reqWithHeaders { requestBody = body }

    do
      -- liftIO $ print cj
      
      result <- liftIO $ try (httpLbs reqFinal mgr) :: MonadIO m => m (Either SomeException (Response BL.ByteString))
      case result of
        Left err -> throwError (HttpError (show err))
        Right resp -> do
          let
            newCj = responseCookieJar resp
            -- Update state with new cookie jar
          modify (\s -> s & ctxCookieJar .~ newCj)

          -- Optionally log status
          let
            status = responseStatus resp
          liftIO $ print status    
          when (not (statusIsSuccessful status)) $
            throwError (HttpError (show status))
          return resp

-- | Convenience for GET requests
httpGet
    :: (MonadError ErrorKind m, MonadState Ctx m, MonadIO m)
    => String                -- ^ URL
    -> [(HeaderName, B.ByteString)]    -- ^ extra headers
    -> m (Response BL.ByteString)
httpGet url extraHeaders = do
    req <- liftIO $ parseRequest url
    httpRequest req extraHeaders Nothing

-------------------------------------------------------------------------------------------


-- | Split a string on a delimiter character.
splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim s =
    let (chunk, rest) = break (== delim) s
    in chunk : case rest of
         []       -> []
         (_:rest') -> splitOn delim rest'


-- | Extracts the HttpOnly flag and the remaining line without the prefix.
extractHttpOnlyFlag :: String -> (Bool, String)
extractHttpOnlyFlag l
  | "#HttpOnly_" `isPrefixOf` l = (True, drop 10 l)   -- strip the prefix
  | otherwise                   = (False, l)
        
-- | Parse a single line of a Netscape cookie file.
--   Format: domain\tflag\tpath\tsecure\texpiry\tname\tvalue
--   flag : TRUE/FALSE (whether to send to all subdomains)
--   secure : TRUE/FALSE (whether cookie requires HTTPS)
parseNetscapeLine :: UTCTime -> String -> Maybe Cookie
parseNetscapeLine now line
    | null line = Nothing
    | otherwise =
        let
          (httpOnly, rest) = extractHttpOnlyFlag line
          fields = filter (not . null) $ splitOn '\t' rest
        in
          case fields of 
            [domain, flag, path, secure, expiry, name, value] -> do
              let
                host_only = map toLower flag == "false"
                domain' = if host_only
                          then domain                     -- no leading dot for host-only cookies
                          else if head domain == '.'
                               then domain
                               else '.' : domain
                path'   = if null path then "/" else path
                expirySeconds = case reads expiry of { [(n, "")] -> Just n; _ -> Nothing }
                -- For session cookies (expiry == 0) use a far‑future expiry
                expiryTime = case expirySeconds of
                  Just 0  -> addUTCTime (365*24*60*60*10) now   -- 10 years from now
                  Just s  -> posixSecondsToUTCTime (fromIntegral s)
                  Nothing -> addUTCTime (365*24*60*60*10) now   -- treat missing as session
                persistent = maybe False (> 0) expirySeconds
                -- 3. Secure flag
                secure_only = map toLower secure == "true"
                
              pure Cookie
                { cookie_name   = pack name
                , cookie_value  = pack value
                , cookie_domain = pack domain'
                , cookie_path   = pack path'
                , cookie_expiry_time   = expiryTime
                , cookie_creation_time   = now
                , cookie_persistent      = persistent
                , cookie_last_access_time = now
                , cookie_host_only       = host_only
                , cookie_secure_only     = secure_only
                , cookie_http_only = httpOnly
                }
            _ -> Nothing

-- | Parse the whole Netscape cookie file content.
parseNetscapeCookies :: UTCTime -> String -> [Cookie]
parseNetscapeCookies now = catMaybes . map (parseNetscapeLine now) . lines

-- | Execute the bash login script and build a CookieJar.
--   The script must be executable and located in the current directory.
loginAndGetCookies :: (MonadIO m, MonadError ErrorKind m) => String -> String -> m CookieJar
loginAndGetCookies username password = do
    let script = "bash"
        args   = ["/home/prolog/MidnightMover/punbb_login.sh", username, password]

    (exitCode, stdout, stderrOutput) <- liftIO $ readProcessWithExitCode script args ""

    case exitCode of
        ExitFailure code -> throwError . BunkerScript $ "Login script failed with exit code " ++ show code ++ ": " ++ stderrOutput
        ExitSuccess      -> do
            now <- liftIO $ getCurrentTime
            liftIO $ print stdout
            let cookies = parseNetscapeCookies now stdout
            if null cookies
                then throwError . BunkerScript $ "No cookies were returned by the login script."
                else do
                    -- http-client expects a CookieJar; create it from the list
                    -- Note: createCookieJar sorts and merges cookies internally.
                    pure $ createCookieJar cookies


-- | Verify login by accessing the index page and checking status/content.
verifyLogin
    :: (MonadError ErrorKind m, MonadState Ctx m, MonadIO m)
    => m ()
verifyLogin = do
    resp <- httpGet indexUrl []
    liftIO $ BL.writeFile "index.html" (responseBody resp)
    let status = responseStatus resp
    unless (statusIsSuccessful status) $
        throwError (HttpError ("Index page returned " ++ show status))
    -- Optionally parse the response for a "logout" link or username.
    return ()

-- | Простейшая URL-кодировка.
urlEncode :: [(String, String)] -> String
urlEncode = intercalate "&" . map (\(k, v) -> escape k ++ "=" ++ escape v)
  where
    escape :: String -> String
    escape = concatMap encodeChar

    encodeChar :: Char -> String
    encodeChar c
      | c `elem` allowed = [c]
      | otherwise = '%' : toHex (ord c)

    allowed :: String
    allowed = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_.~"

    toHex :: Int -> String
    toHex n = [hexDigit (n `div` 16), hexDigit (n `mod` 16)]

    hexDigit :: Int -> Char
    hexDigit i
      | i < 10 = chr (ord '0' + i)
      | otherwise = chr (ord 'A' + i - 10)


postReply :: (MonadError ErrorKind m, MonadState Ctx m, MonadIO m) => String -> Int -> Text -> m ()
postReply username tid msg = do
  -- 2. Сформировать POST-запрос
  let postUrl = "https://gamestories.clanboard.ru/post.php?tid=" ++ show tid
  reqBase <- liftIO $ parseRequest postUrl
  let params = [ ("form_sent", "1")
               , ("form_user", username)
               , ("req_message", T.unpack msg)
               , ("submit", "Отправить")
               ]
      body = pack $ urlEncode params
      req = reqBase
            { method = "POST"
            , requestHeaders = [ (hContentType, "application/x-www-form-urlencoded") ]
            , requestBody = RequestBodyBS body
            }
    
  -- 3. Отправить
  resp <- httpRequest req [] Nothing
  let status = responseStatus resp
  unless (statusIsSuccessful status) $
    throwError (HttpError ("Post reply failed: " ++ show status))
  liftIO $ BL.writeFile "afterpost.html" (responseBody resp)

loginBunker
    :: (MonadError ErrorKind m, MonadState Ctx m, MonadIO m)
    => String -> String -> m ()
loginBunker login password =
  do
    tokenTest

{-
    
    cks <- loginAndGetCookies login password
    liftIO $ print cks
    modify (\s -> s & ctxCookieJar .~ cks)
    verifyLogin
    postReply login 22 "Slow down, back off\nTell him you don't pay the price"

-}

{-

import Control.Exception (try, SomeException)
import Control.Lens hiding (element)
import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import Data.Encoding (encodeStrictByteString)
import Data.Encoding.CP1251

import Data.List (intersperse)
import Data.Text (Text)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as B 
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE


import Network.HTTP.Client
import Network.HTTP.Types.Header (hReferer, hContentType, HeaderName)
import Network.HTTP.Types.Method (methodPost)
import Network.HTTP.Types.Status (statusIsSuccessful)
import Network.HTTP.Types.URI (urlEncode)

import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (fromDocument)

import Ctx
-- import Parse

-- | Constants
userAgent :: B.ByteString
userAgent = "Mozilla/5.0 (X11; Linux x86_64; rv:146.0) Gecko/20100101 Firefox/146.0" -- "MidnightMover/0.0"

loginUrl :: String
loginUrl = "https://gamestories.clanboard.ru/login.php"


encodeWindows1251 :: String -> B.ByteString
encodeWindows1251 = encodeStrictByteString CP1251

-- | Execute an HTTP request, update the cookie jar in the state,
--   and return the response.  Throws 'ErrorKind' on failure.
httpRequest
    :: (MonadError ErrorKind m, MonadState Ctx m, MonadIO m)
    => Request         -- ^ base request (method, url, etc.)
    -> [(HeaderName, B.ByteString)] -- ^ extra headers
    -> Maybe RequestBody   -- ^ optional request body
    -> m (Response ByteString)
httpRequest baseReq extraHeaders mBody = do
    ctx <- get
    let
        mgr = _ctxManager ctx
        cj  = _ctxCookieJar ctx

        -- Add headers and cookie jar to the request
        reqWithHeaders = baseReq
            {  
              requestHeaders = extraHeadersStd ++ extraHeaders
            , cookieJar = Just cj
            }
        extraHeadersStd = [ ("User-Agent", userAgent), ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8") ]

        reqFinal = case mBody of
            Nothing -> reqWithHeaders
            Just body -> reqWithHeaders { requestBody = body }

    do
      liftIO $ print cj
      
      result <- liftIO $ try (httpLbs reqFinal mgr) :: MonadIO m => m (Either SomeException (Response ByteString))
      case result of
        Left err -> throwError (HttpError (show err))
        Right resp -> do
          let
            newCj = responseCookieJar resp
            -- Update state with new cookie jar
          modify (\s -> s & ctxCookieJar .~ newCj)

          -- Optionally log status
          let
            status = responseStatus resp
          liftIO $ print status    
          when (not (statusIsSuccessful status)) $
            throwError (HttpError (show status))
          return resp

-- | Convenience for GET requests
httpGet
    :: (MonadError ErrorKind m, MonadState Ctx m, MonadIO m)
    => String                -- ^ URL
    -> [(HeaderName, B.ByteString)]    -- ^ extra headers
    -> m (Response ByteString)
httpGet url extraHeaders = do
    req <- liftIO $ parseRequest url
    httpRequest req extraHeaders Nothing

-- | Convenience for POST application/x-www-form-urlencoded
httpPostForm
    :: (MonadError ErrorKind m, MonadState Ctx m, MonadIO m)
    => String                       -- ^ URL
    -> [(HeaderName, B.ByteString)] -- ^ extra headers
    -> [(String, String)]           -- ^ form fields
    -> m (Response ByteString)
httpPostForm url extraHeaders fields = do
    req0 <- liftIO $ parseRequest url
    let req = req0
            { method = methodPost
            , redirectCount = 0
            , checkResponse = \_ _ -> return ()
            , requestHeaders = (hContentType, "application/x-www-form-urlencoded") : extraHeaders
            }
        body = formUrlEncodedBody fields
    httpRequest req [] (Just body)
  where
    formUrlEncodedBody :: [(String, String)] -> RequestBody
    formUrlEncodedBody fields = 
      let toCp1251 = encodeWindows1251  -- defined above
          encField (name, value) = 
            BC.concat [ urlEncode True (toCp1251 name)
                      , "="
                      , urlEncode True (toCp1251 value) ]
          bodyBS = BC.intercalate "&" (map encField fields)
      in RequestBodyLBS (BL.fromStrict bodyBS)

-- | Get initial cookies by visiting the login page.
getInitialCookies
    :: (MonadError ErrorKind m, MonadState Ctx m, MonadIO m)
    => m ()
getInitialCookies = do
    _ <- httpGet loginUrl []  -- response body discarded, only cookie jar updated
    return ()

-- | Perform the login POST.
doLogin
    :: (MonadError ErrorKind m, MonadState Ctx m, MonadIO m)
    => String -> String -> m ()
doLogin login password = do
    let referer = TE.encodeUtf8 . T.pack $ loginUrl -- ++ "?action=in"
        fields =
            [ ("form_sent", "1")
            , ("redirect_url", "/")      -- empty, as in the HTML (index.php)
            , ("req_username", login)
            , ("req_password", password)
            , ("login", "Войти")        -- submit button name & value
            ]
            
        extraHeaders = [(hReferer, referer), ("Origin", "https://gamestories.clanboard.ru")]

    resp <- httpPostForm (loginUrl ++ "?action=in") extraHeaders fields

    -- Optional: check response for success indicator (e.g., "Welcome" text)
    let body = responseBody resp
    liftIO $ BL.writeFile "login_debug.html" body  -- keep for debugging, but could be removed
    -- You could parse the page to confirm login success and throw an error if not.

-- | Verify login by accessing the index page and checking status/content.
verifyLogin
    :: (MonadError ErrorKind m, MonadState Ctx m, MonadIO m)
    => m ()
verifyLogin = do
    resp <- httpGet indexUrl []
    liftIO $ BL.writeFile "index.html" (responseBody resp)
    let status = responseStatus resp
    unless (statusIsSuccessful status) $
        throwError (HttpError ("Index page returned " ++ show status))
    -- Optionally parse the response for a "logout" link or username.
    return ()

-- | Main login function.
loginBunker
    :: (MonadError ErrorKind m, MonadState Ctx m, MonadIO m)
    => String -> String -> m ()
loginBunker login password = do
    getInitialCookies
    doLogin login password
    verifyLogin
     -- If all passes, login is successful.

-}
















{-

{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (bracket)
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Char (isSpace)
import Data.List (splitWhen)
import Data.Maybe (catMaybes)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Cookie (Cookie(..), SetCookie(..), defaultCookie)
import Network.HTTP.Client (CookieJar, createCookieJar, destroyCookieJar)
import Network.HTTP.Client.TLS (getGlobalManager)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

-- | Parse a single line of a Netscape cookie file.
--   Format: domain\tflag\tpath\tsecure\texpiry\tname\tvalue
--   flag : TRUE/FALSE (whether to send to all subdomains)
--   secure : TRUE/FALSE (whether cookie requires HTTPS)
parseNetscapeLine :: String -> Maybe Cookie
parseNetscapeLine line =
    case filter (not . null) $ splitWhen (== '\t') line of
        [domain, _, path, secure, expiry, name, value] -> do
            let domain' = if head domain == '.' then domain else '.' ++ domain  -- ensure leading dot for domain match
                secure' = map toLower secure == "true"
                path'   = if null path then "/" else path
                expirySeconds :: Maybe Integer
                expirySeconds = case reads expiry of
                    [(n, "")] -> Just n
                    _         -> Nothing
                -- Convert expiry (Unix timestamp) to UTCTime if > 0
                expiryTime = posixSecondsToUTCTime . fromInteger <$> expirySeconds
            pure Cookie
                { cookie_name   = name
                , cookie_value  = value
                , cookie_domain = domain'
                , cookie_path   = path'
                , cookie_expiry = expiryTime
                , cookie_secure = secure'
                , cookie_http_only = False          -- Netscape format does not store HttpOnly
                }
        _ -> Nothing

-- | Parse the whole Netscape cookie file content.
parseNetscapeCookies :: String -> [Cookie]
parseNetscapeCookies = catMaybes . map parseNetscapeLine . filter (not . isComment) . lines
  where
    isComment ('#':_) = True
    isComment ""      = True
    isComment _       = False

-- | Execute the bash login script and build a CookieJar.
--   The script must be executable and located in the current directory.
loginAndGetCookies :: String -> String -> IO CookieJar
loginAndGetCookies username password = do
    let script = "./punbb_login.sh"
        args   = [username, password]

    (exitCode, stdout, stderrOutput) <- readProcessWithExitCode script args ""

    case exitCode of
        ExitFailure code -> fail $ "Login script failed with exit code " ++ show code ++ ": " ++ stderrOutput
        ExitSuccess      -> do
            let cookies = parseNetscapeCookies stdout
            if null cookies
                then fail "No cookies were returned by the login script."
                else do
                    -- http-client expects a CookieJar; create it from the list
                    -- Note: createCookieJar sorts and merges cookies internally.
                    pure $ createCookieJar cookies

-- Example usage (inside an IO action, e.g. main):
-- main :: IO ()
-- main = do
--     manager <- getGlobalManager
--     cookieJar <- loginAndGetCookies "my_username" "my_password"
--     -- Now you can use 'cookieJar' with http-client requests (e.g., with a cookie jar manager)
--     -- e.g. let request = ... ; response <- httpLbs (applyCookies cookieJar request) manager
--     return ()

-}
