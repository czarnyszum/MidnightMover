{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bunker (loginBunker) where

import Control.Concurrent
import Control.Lens hiding (element)
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.IO.Class

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Test.WebDriver
import Test.WebDriver.Commands
import Test.WebDriver.Config

import Ctx

-- | Default WebDriver configuration using ChromeDriver
punbbConfig :: WDConfig
punbbConfig = defaultConfig
  { wdHost = "localhost"
  , wdPort = 4444
  }

savePageAsHtml :: FilePath -> WD ()
savePageAsHtml filePath = do
  pageSource <- getSource
  liftIO $ T.writeFile filePath pageSource

-- | Login to PunBB forum
-- Takes base URL, username and password
loginPunBB :: Text -> Text -> Text -> WD ()
loginPunBB baseUrl username password = do
  -- Navigate to login page
  openPage $ T.unpack baseUrl <> "/login.php"
  
  -- Wait for and fill in username field
  usernameField <- findElem (ById "fld1")
  clearInput usernameField
  sendKeys username usernameField
  
  -- Fill in password field
  passwordField <- findElem (ById "fld2")
  clearInput passwordField
  sendKeys password passwordField
  
  -- Submit the login form
  submitBtn <- findElem (ByName "login")
  click submitBtn
  
  -- Wait for redirect after login
  liftIO $ threadDelay 2000000 -- 2 second delay

-- | Extract token value from hidden input within a div
-- PunBB's process_form() appends hidden inputs to formkey and formetc divs
getTokenFromDiv :: Text -> WD (Text, Text)
getTokenFromDiv divId = do
  -- Find the div containing the token
  tokenDiv <- findElem (ById divId) -- (ById $ T.unpack divId)
  
  -- Find the hidden input inside the div
  -- process_form() appends an <input type="hidden"> to these divs
  hiddenInput <- findElemFrom tokenDiv (ByTag "input")
  
  -- Get both name and value attributes of the hidden input
  tokenName <- attr hiddenInput "name"
  tokenValue <- attr hiddenInput "value"
  
  case (tokenName, tokenValue) of
    (Just n, Just v) -> return (n, v)
    _ -> error $ "Could not find token in div: " <> T.unpack divId

-- | Trigger process_form() by attempting to submit, then capture tokens
-- We need to execute process_form() to populate the hidden fields
triggerProcessForm :: WD ()
triggerProcessForm = do
  -- Execute process_form via JavaScript to populate hidden fields
  -- without actually submitting the form
  executeJS [] 
    "var form = document.getElementById('post');\
    \process_form(form);" :: WD ()

-- | Get both security tokens after triggering process_form
getSecurityTokens :: WD ((Text, Text), (Text, Text))
getSecurityTokens = do
  -- Trigger process_form to populate the hidden divs
  triggerProcessForm
  
  -- Small delay to ensure DOM is updated
  liftIO $ threadDelay 500000 -- 0.5 second delay
  
  -- Extract tokens from both divs
  formkeyToken <- getTokenFromDiv "formkey"
  formetcToken <- getTokenFromDiv "formetc"
  
  return (formkeyToken, formetcToken)
 
-- | Post a message to a PunBB thread
-- Takes base URL, thread ID, and message content
postMessage :: Text -> Int -> Text -> WD ()
postMessage baseUrl threadId message = do
  -- Navigate to the post reply page
  openPage $ T.unpack baseUrl <> "/post.php?tid=" <> show threadId
  
  -- Wait for page to load
  liftIO $ threadDelay 1000000 -- 1 second delay
  
  -- Find and fill in the message textarea
  -- PunBB uses 'req_message' as the textarea name
  messageArea <- findElem (ByName "req_message")
  clearInput messageArea
  sendKeys message messageArea
  
  -- Trigger process_form to populate security tokens
  -- We call it via JS before clicking submit
  triggerProcessForm
  
  -- Small delay for token population
  liftIO $ threadDelay 500000
  
  -- Verify tokens were populated (optional but useful for debugging)
  (formkeyName, formkeyVal) <- getTokenFromDiv "formkey"
  (formetcName, formetcVal) <- getTokenFromDiv "formetc"
  
  liftIO $ putStrLn $ "FormKey token - Name: " <> T.unpack formkeyName 
                    <> ", Value: " <> T.unpack formkeyVal
  liftIO $ putStrLn $ "Formetc token - Name: " <> T.unpack formetcName 
                    <> ", Value: " <> T.unpack formetcVal
  
  -- Now click the submit button

-- The onsubmit handler will call process_form again, but tokens are already set
  submitBtn <- findElem (ByName "submit")
  click submitBtn
  
  -- Wait for post to complete
  liftIO $ threadDelay 2000000 -- 2 second delay



{-



-- | Alternative: Submit form entirely via JavaScript
-- Useful if the normal submit flow has issues
postMessageViaJS :: Text -> Int -> Text -> WD ()
postMessageViaJS baseUrl threadId message = do
  openPage $ T.unpack baseUrl <> "/post.php?tid=" <> show threadId
  liftIO $ threadDelay 1000000
  
  -- Fill message via JavaScript to avoid any input issues
  executeJS [JSArg message] 
    "document.getElementsByName('req_message')[0].value = arguments[0];" :: WD ()
  
  -- Trigger process_form to set security tokens
  triggerProcessForm
  liftIO $ threadDelay 500000
  
  -- Submit form via JavaScript
  executeJS []
    "var form = document.getElementById('post');\
    \form.submit();" :: WD ()
  
  liftIO $ threadDelay 2000000

-- | Complete bot session: login and post a message
runPunBBBot :: Text -- ^ Base URL (e.g., "http://forum.example.com")
            -> Text -- ^ Username
            -> Text -- ^ Password  
            -> Int -- ^ Thread ID
            -> Text -- ^ Message to post
            -> IO ()
runPunBBBot baseUrl username password threadId message = do
  runSession punbbConfig $ do
    -- Login first
    loginPunBB baseUrl username password
    
    -- Post the message
    postMessage baseUrl threadId message
    
    -- Close the session
    closeSession

-- | Run multiple posts in a single session
runPunBBBotMultiple :: Text -- ^ Base URL
                    -> Text -- ^ Username
                    -> Text -- ^ Password
                    -> [(Int, Text)] -- ^ List of (threadId, message) pairs
                    -> IO ()
runPunBBBotMultiple baseUrl username password posts = do
  runSession punbbConfig $ do
    loginPunBB baseUrl username password
    
    mapM_ (\(tid, msg) -> do
      postMessage baseUrl tid msg
      liftIO $ threadDelay 1000000 -- 1 second between posts
      ) posts
    
    closeSession
-}


bunkerUrl :: String
bunkerUrl = "https://gamestories.clanboard.ru"

loginBunker
    :: (MonadError ErrorKind m, MonadState Ctx m, MonadIO m)
    => String -> String -> m ()
loginBunker login password =
  do
    let
      base = T.pack bunkerUrl
    liftIO $ runSession punbbConfig $ do

      loginPunBB base (T.pack login) (T.pack password)    
      postMessage base 22 "Hey you - come on!\nI show you something\nThere is what it takes for you\nMmh, you better follow me" 

      -- savePageAsHtml "test_login.html"
      closeSession

  
    return ()


{- 

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
    r <- tokenTest
    return ()


-}
