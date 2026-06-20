{-# LANGUAGE OverloadedStrings #-}

module Antibot (loadOnlyProcessFormScript) where

import Test.WebDriver
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Simple
import Control.Monad.IO.Class
import Control.Exception

--------------------------------------------------------------------------------
-- Browser capabilities: Chrome with JavaScript disabled
--------------------------------------------------------------------------------

chromeNoJsCaps :: Capabilities
chromeNoJsCaps = defaultCaps
  { browser = chrome
  , additionalCaps =
      [ "pageLoadStrategy" .= ("eager" :: Text)
      , "goog:chromeOptions" .= object
          [ "prefs" .= object
              [ -- 1 = allow, 2 = block
                "profile.managed_default_content_settings.javascript" .= (2 :: Int)
              , "profile.default_content_setting_values.javascript" .= (2 :: Int)
              ]
          ]
      ]
  }

--------------------------------------------------------------------------------
-- Download external JS file as Text
--------------------------------------------------------------------------------

downloadText :: Text -> IO Text
downloadText url = do
  req <- parseRequest (T.unpack url)
  resp <- httpLBS req

  let statusCode = getResponseStatusCode resp

  if statusCode >= 200 && statusCode < 300
    then pure $
      TE.decodeUtf8 $
        LBS.toStrict $
          getResponseBody resp
    else error $
      "Failed to download script: "
        ++ T.unpack url
        ++ ", HTTP status: "
        ++ show statusCode

--------------------------------------------------------------------------------
-- Get all external <script src="..."> from <head>
--------------------------------------------------------------------------------

getHeadExternalScriptUrls :: WD [Text]
getHeadExternalScriptUrls =
  executeJS
    "return Array.from(document.querySelectorAll('head script[src]')).map(s => s.src);"
    ([] :: [Value])

--------------------------------------------------------------------------------
-- Get all inline <script>...</script> contents from <head>
--------------------------------------------------------------------------------

getHeadInlineScripts :: WD [Text]
getHeadInlineScripts =
  executeJS
    "return Array.from(document.querySelectorAll('head script:not([src])')).map(s => s.textContent || '');"
    ([] :: [Value])

--------------------------------------------------------------------------------
-- Find external script containing process_form
--------------------------------------------------------------------------------

findExternalProcessFormScript :: [Text] -> WD (Maybe Text)
findExternalProcessFormScript [] = pure Nothing
findExternalProcessFormScript (url:rest) = do
  liftIO $ putStrLn $ "Checking external script: " ++ T.unpack url

  result <- liftIO $ try $ downloadText url :: WD (Either SomeException Text)

  case result of
    Left err -> do
      liftIO $ putStrLn $ "Could not download script: " ++ show err
      findExternalProcessFormScript rest

    Right body ->
      if "process_form" `T.isInfixOf` body
        then do
          liftIO $ putStrLn $ "Found process_form in: " ++ T.unpack url
          pure $ Just body
        else
          findExternalProcessFormScript rest

--------------------------------------------------------------------------------
-- Find inline script containing process_form
--------------------------------------------------------------------------------

findInlineProcessFormScript :: [Text] -> Maybe Text
findInlineProcessFormScript scripts =
  case filter (T.isInfixOf "process_form") scripts of
    []    -> Nothing
    x : _ -> Just x

--------------------------------------------------------------------------------
-- Execute downloaded/inline JS and expose window.process_form
--------------------------------------------------------------------------------

executeProcessFormScript :: Text -> WD Text
executeProcessFormScript js = do
  let wrappedJs =
        T.concat
          [ js
          , "\n\n"
          , ";"
          , "\nif (typeof process_form === 'function') {"
          , "\n  window.process_form = process_form;"
          , "\n}"
          , "\nif (typeof window.process_form === 'function') {"
          , "\n  return 'process_form loaded';"
          , "\n}"
          , "\nthrow new Error('Script was executed, but process_form was not found');"
          ]

  executeJS wrappedJs ([] :: [Value])

--------------------------------------------------------------------------------
-- Main helper: find and load only process_form script
--------------------------------------------------------------------------------

loadOnlyProcessFormScript :: WD ()
loadOnlyProcessFormScript = do
  externalUrls <- getHeadExternalScriptUrls
  inlineScripts <- getHeadInlineScripts

  liftIO $ putStrLn $ "External head scripts found: " ++ show (length externalUrls)
  liftIO $ putStrLn $ "Inline head scripts found: " ++ show (length inlineScripts)

  externalScript <- findExternalProcessFormScript externalUrls

  let scriptToExecute =
        case externalScript of
          Just js -> Just js
          Nothing -> findInlineProcessFormScript inlineScripts

  case scriptToExecute of
    Nothing ->
      error "Could not find script containing process_form"

    Just js -> do
      result <- executeProcessFormScript js
      liftIO $ TIO.putStrLn result
