{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Lens
import Control.Monad.State
import Control.Monad.Except

import Ctx

type AppM = ExceptT ErrorKind (StateT Ctx IO)

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
        (res, _) <- runAppM (move u) ctx
        putStrLn $ "Result: " ++ show res
        putStrLn $ "Success"
