{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Post (Post, extractPost, savePost, showB) where

import Control.Monad.IO.Class

import Data.ByteString.Lazy (ByteString)
import Data.Sequence (Seq)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Sequence as S
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import Data.Either (isRight)
import Data.Foldable

import Text.HTML.TagSoup

data PostElement = PostImage ByteString | PostLine ByteString | PostLineBreak

showB :: ByteString -> String
showB = T.unpack . T.decodeUtf8

instance Show PostElement where
  show (PostImage src) = "Img[" ++ (showB src) ++ "]" 
  show (PostLine l) = showB l
  show PostLineBreak = "\n"

data PostExtractor =
  LookingForArticle
  | LookingForLines (Seq PostElement)
  | Stop (Seq PostElement)

valid :: BL.ByteString -> Bool
valid bs =
    not (BL.null bs)          -- 1) не пустой
    && isValidUtf8 bs         -- 2) валидный UTF‑8

isValidUtf8 :: BL.ByteString -> Bool
isValidUtf8 = isRight . T.decodeUtf8'

isBBCodeImage :: [Attribute ByteString] -> Maybe PostElement
isBBCodeImage attrs =
  do
    cls <- lookup "class" attrs
    if "bbCodeImage" `elem` B.words (BL.toStrict cls)
    then fmap PostImage $ lookup "src" attrs
    else Nothing

analyseImage :: [Attribute ByteString] -> Maybe PostElement
analyseImage attrs = isBBCodeImage attrs

-- bbCodeImage LbImage

trans :: PostExtractor -> Tag ByteString -> PostExtractor
trans LookingForArticle (TagOpen "article" _) = LookingForLines S.empty
trans (LookingForLines xs) (TagOpen "br" _) = LookingForLines (xs S.|> PostLineBreak)
trans (LookingForLines xs) (TagOpen "img" attrs) =
  case analyseImage attrs of
    Just x -> LookingForLines (xs S.|> x)
    Nothing -> LookingForLines xs
trans (LookingForLines xs) (TagText x) | valid x = LookingForLines (xs S.|> PostLine x)
trans (LookingForLines xs) (TagText x) = LookingForLines xs
trans (LookingForLines xs) (TagClose "article") = Stop xs                                         
trans s _ = s

type Post = Seq PostElement

extractPost :: Seq (Tag ByteString) -> Maybe Post
extractPost tags =
  let
    state = foldl' trans LookingForArticle tags
  in
    case state of
      Stop xs -> Just xs
      _ -> Nothing  

savePost :: (MonadIO m) => String -> Post -> m ()
savePost prefix p =
  do
    let
      ps = concatMap show (toList p)
      nm = prefix ++ ".txt"
    liftIO $ writeFile nm ps

