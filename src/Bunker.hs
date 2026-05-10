{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bunker (loginBunker) where

import Control.Exception (try, SomeException)
import Control.Lens hiding (element)
import Control.Monad
import Control.Monad.Except
import Control.Monad.State

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
userAgent = "MidnightMover/0.0"

loginUrl :: String
loginUrl = "https://gamestories.clanboard.ru/login.php"

indexUrl :: String
indexUrl = "https://gamestories.clanboard.ru/index.php"

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
    let mgr = _ctxManager ctx
        cj  = _ctxCookieJar ctx

        -- Add headers and cookie jar to the request
        reqWithHeaders = baseReq
            { requestHeaders = extraHeadersStd ++ extraHeaders
            , cookieJar = Just cj
            }
        extraHeadersStd = [ ("User-Agent", userAgent) ]

        reqFinal = case mBody of
            Nothing -> reqWithHeaders
            Just body -> reqWithHeaders { requestBody = body }

    do
      result <- liftIO $ try (httpLbs reqFinal mgr) :: MonadIO m => m (Either SomeException (Response ByteString))
      case result of
        Left err -> throwError (HttpError (show err))
        Right resp -> do
          let
            newCj = responseCookieJar resp
            -- Update state with new cookie jar
          modify (\s -> s & ctxCookieJar .~ newCj)
            -- Optionally log status
          when (not (statusIsSuccessful (responseStatus resp))) $
            throwError (HttpError (show (responseStatus resp)))
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
            , requestHeaders = (hContentType, "application/x-www-form-urlencoded") : extraHeaders
            }
        body = formUrlEncodedBody fields
    httpRequest req [] (Just body)
  where
    formUrlEncodedBody :: [(String, String)] -> RequestBody
    formUrlEncodedBody kvs =
      let pairs = map (\(k, v) -> BC.concat [ urlEncode True (BC.pack k)
                                       , "="
                                       , urlEncode True (BC.pack v) ])
                  kvs
          bodyBS = BC.intercalate "&" pairs
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
    let referer = TE.encodeUtf8 . T.pack $ loginUrl
        fields =
            [ ("form_sent", "1")
            , ("redirect_url", "")      -- empty, as in the HTML (index.php)
            , ("req_username", login)
            , ("req_password", password)
            , ("login", "Войти")        -- submit button name & value
            ]
            
        extraHeaders = [(hReferer, referer)]

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
