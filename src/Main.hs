{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import Control.Lens
import Control.Monad.State
import Control.Monad.Except
 
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO as T

import GHC.Generics (Generic)

import System.FilePath ((</>))

import Ctx
import Bunker

type AppM = ExceptT ErrorKind (StateT Ctx IO)

data Desc = Desc {
  _descMsgs :: [String]
} deriving (Generic)
makeLenses ''Desc

instance ToJSON Desc
instance FromJSON Desc

readDesc :: String -> IO (Maybe Desc)
readDesc basePath = do
  let path = basePath </> "desc.json"
  c <- BL.readFile path
  case eitherDecode c of
    Left _err   -> return Nothing
    Right user  -> return (Just user)

writeDesc :: String -> Desc -> IO ()
writeDesc basePath d =
  do
    let
      path = basePath </> "desc.json"
      content = encode d
    BL.writeFile path content

runAppM :: AppM a -> Ctx -> IO (Either ErrorKind a, Ctx)
runAppM m ctx = runStateT (runExceptT m) ctx
    
main :: IO ()
main = do
  mUser <- readUser "."
  case mUser of
    Nothing -> putStrLn "Failed to read user.txt"
    Just u  ->
      do
        
        ctx <- emptyCtx
        case u ^. userOutput of
          OutputFile ->
            do
              let job =
                    do
                      msgs <- getMessages u
                      let
                        desc = Desc msgs
                      liftIO $ writeDesc "." desc
              _ <- runAppM job ctx
              return ()  
          OutputBunker login password ->
            do
              print (login, password)
              let
                job =
                  do
                    maybeDesc <- liftIO $ readDesc "."
                    case maybeDesc of
                      Just desc ->
                        do
                          let
                            ps = view descMsgs desc
                          msgs <- mapM (liftIO . T.readFile) ps
                          copyToBunker login password msgs
                      Nothing -> return ()    
              _ <- runAppM job ctx
              return ()
        putStrLn "End"  
        
        
