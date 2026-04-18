{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Post (Post, extractPost, savePost, showB) where

-- import Control.Monad
import Control.Monad.IO.Class

import Data.ByteString.Lazy (ByteString)
import Data.Sequence (Seq)
-- import qualified Data.ByteString.Char8 as B
-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Sequence as S
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
-- import Data.Either (isRight)
import Data.Foldable

import Text.XML.Cursor

data Alignment = Unaligned | Centered deriving (Show)

data PostElement =
  PostImage ByteString
  | PostLine Alignment ByteString
  | PostLineBreak
  | PostSpoiler ByteString (Seq PostElement)

showB :: ByteString -> String
showB = T.unpack . T.decodeUtf8

instance Show PostElement where
  show (PostImage src) = "Img[" ++ (showB src) ++ "]" 
  show (PostLine a l) = "Text-" ++ show a ++ "-[" ++ showB l ++ "]"
  show PostLineBreak = ""
  show (PostSpoiler title body) = "Spolier[" ++ showB title ++ "][" ++ (concatMap show (toList body)) ++ "]"

type Post = Seq PostElement

savePost :: (MonadIO m) => String -> Post -> m ()
savePost prefix p =
  do
    let
      ps = concatMap show (toList p)
      nm = prefix ++ ".txt"
    liftIO $ writeFile nm ps

extractPost :: Cursor -> Maybe Post
extractPost = undefined


{-

data SpolierExtractor =
  LookingForSpoilerTitle
  | ExtractSpoilerTitle
  | ExtractingSpolier ByteString Int PostExtractor
  | StopSpolier ByteString (Seq PostElement)

isSpolierExtracted :: SpolierExtractor -> Maybe (ByteString, Seq PostElement)
isSpolierExtracted (StopSpolier title content) = Just (title, content)
isSpolierExtracted _ = Nothing

data PostExtractor =
  LookingForArticle
  | LookingForLines (Seq PostElement)
  | Spoiler (Seq PostElement) SpolierExtractor
  | Stop (Seq PostElement)

--  bbCodeSpoilerText

valid :: BL.ByteString -> Bool
valid bs =
    not (BL.null bs)
    && isValidUtf8 bs

isValidUtf8 :: BL.ByteString -> Bool
isValidUtf8 = isRight . T.decodeUtf8'

isBBCodeImage :: [Attribute ByteString] -> Maybe PostElement
isBBCodeImage attrs =
  do
    cls <- lookup "class" attrs
    if "bbCodeImage" `elem` B.words (BL.toStrict cls)
    then fmap PostImage $ lookup "src" attrs
    else Nothing

isSpoilerStart :: [Attribute ByteString] -> Maybe ()
isSpoilerStart attrs =
  do
    cls <- lookup "class" attrs
    if "bbCodeSpoilerContainer" `elem` B.words (BL.toStrict cls)
    then return ()
    else mzero 


--  ToggleTriggerAnchor bbCodeSpoilerContainer

-- SpoilerTarget bbCodeSpoilerText

analyseImage :: [Attribute ByteString] -> Maybe PostElement
analyseImage attrs = isBBCodeImage attrs

-- bbCodeImage LbImage

extractSpoilerTitle :: [Attribute ByteString] -> Bool
extractSpoilerTitle attrs =
  case lookup "class" attrs of
    Just cls -> "SpoilerTitle" `elem` B.words (BL.toStrict cls)
    Nothing  -> False
      
strans :: SpolierExtractor -> Tag ByteString -> SpolierExtractor
strans LookingForSpoilerTitle (TagOpen "span" attrs) =
  if extractSpoilerTitle attrs
  then ExtractSpoilerTitle
  else LookingForSpoilerTitle
strans LookingForSpoilerTitle _ = LookingForSpoilerTitle
strans ExtractSpoilerTitle (TagText x) = ExtractingSpolier x 0 (LookingForLines S.empty)
strans ExtractSpoilerTitle _ = ExtractSpoilerTitle 
strans (ExtractingSpolier title divs mach) (TagOpen "div" attrs) =
  ExtractingSpolier title (divs + 1) (trans mach (TagOpen "div" attrs)) 
strans (ExtractingSpolier title 0 mach) (TagClose "div") =
  case mach of
    LookingForLines xs -> StopSpolier title xs
    Stop xs            -> StopSpolier title xs
    _                  -> StopSpolier title S.empty -- error state
strans (ExtractingSpolier title divs mach) (TagClose "div") =
  ExtractingSpolier title (divs - 1) (trans mach (TagClose "div")) 
strans (ExtractingSpolier title divs mach) tag = ExtractingSpolier title divs (trans mach tag)
strans (StopSpolier title xs) _ = StopSpolier title xs
  
trans :: PostExtractor -> Tag ByteString -> PostExtractor
trans (Spoiler xs mach) tag =
  let
    mach' = strans mach tag
  in
    case isSpolierExtracted mach' of
      Just (title, spoiler) -> LookingForLines (xs S.|> PostSpoiler title spoiler)
      Nothing -> Spoiler xs mach'
trans LookingForArticle (TagOpen "article" _) = LookingForLines S.empty
trans (LookingForLines xs) (TagOpen "br" _) = LookingForLines (xs S.|> PostLineBreak)
trans (LookingForLines xs) (TagOpen "img" attrs) =
  case analyseImage attrs of
    Just x -> LookingForLines (xs S.|> x)
    Nothing -> LookingForLines xs
trans (LookingForLines xs) (TagOpen "div" attrs) =
  case isSpoilerStart attrs of
    Just _ -> Spoiler xs LookingForSpoilerTitle
    Nothing -> LookingForLines xs
trans (LookingForLines xs) (TagText x) | valid x = LookingForLines (xs S.|> PostLine x)
trans (LookingForLines xs) (TagText x) = LookingForLines xs
trans (LookingForLines xs) (TagClose "article") = Stop xs                                         
trans s _ = s


-}
