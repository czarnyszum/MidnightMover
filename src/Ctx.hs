{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Ctx where

import Control.Monad.State
import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import System.FilePath ((</>))

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

{-
data Ctx = Ctx {
               }

makeLenses ''Ctx

-}
