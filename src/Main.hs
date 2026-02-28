{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Lens
import Control.Monad.State
import Control.Monad.Except

import Network.HTTP.Client (createCookieJar)

import Ctx

type AppM = ExceptT ErrorKind (StateT Ctx IO)

runAppM :: AppM a -> Ctx -> IO (Either ErrorKind a, Ctx)
runAppM m ctx = runStateT (runExceptT m) ctx

job :: User -> AppM ()
job user =
  do
    let
      threads = view userThreads user
      thread0 = threads !! 0
      f (x, y, _) = liftIO $ print (x, y)
    login user
    xs <- getPageMessages thread0
    liftIO $ print (length xs)
    mapM_ f xs
    
main :: IO ()
main = do
  mUser <- readUser "."
  case mUser of
    Nothing -> putStrLn "Failed to read user.txt"
    Just u  ->
      do
        
        ctx <- emptyCtx
        (res, finalCtx) <- runAppM (job u) ctx
        putStrLn $ "Result: " ++ show res
        putStrLn $ "Success"
