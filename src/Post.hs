{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Post where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Char8 as B
import Data.Sequence (Seq)
import qualified Data.Sequence as S

data PostElement = PostImage ByteString | PostLine ByteString

showB :: ByteString -> String
showB = T.unpack . T.decodeUtf8

instance Show PostElement where
  show (PostImage src) = "Image: " ++ (showB src) 
