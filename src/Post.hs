{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Post where

import Data.ByteString.Lazy (ByteString)
import Data.Sequence (Seq)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Sequence as S
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

data PostElement = PostImage ByteString | PostLine ByteString

showB :: ByteString -> String
showB = T.unpack . T.decodeUtf8

instance Show PostElement where
  show (PostImage src) = "Image: " ++ (showB src) 
