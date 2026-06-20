{-# LANGUAGE OverloadedStrings #-}

module Antibot (loadOnlyProcessFormScript) where

import Data.Aeson
import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS

import Control.Monad.IO.Class
import Control.Exception

import Test.WebDriver

--------------------------------------------------------------------------------
-- Get all inline <script>...</script> contents from <head>
--------------------------------------------------------------------------------

getHeadInlineScripts :: WD [Text]
getHeadInlineScripts =
  executeJS []
    "return Array.from(document.querySelectorAll('head script:not([src])')).map(s => s.textContent || '');"


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

  executeJS [] wrappedJs

--------------------------------------------------------------------------------
-- Main helper: find and load only process_form script
--------------------------------------------------------------------------------

loadOnlyProcessFormScript :: WD ()
loadOnlyProcessFormScript = do
  inlineScripts <- getHeadInlineScripts

  liftIO $ putStrLn $ "Inline head scripts found: " ++ show (length inlineScripts)

  let scriptToExecute = findInlineProcessFormScript inlineScripts

  case scriptToExecute of
    Nothing ->
      error "Could not find script containing process_form"

    Just js -> do
      result <- executeProcessFormScript js
      liftIO $ TIO.putStrLn result
