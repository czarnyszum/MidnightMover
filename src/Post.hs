{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Post (Post, isValidPost, extractPost, savePost, exractPageNumber) where

-- import Control.Monad
import Control.Monad.IO.Class

-- import Data.ByteString.Lazy (ByteString)
-- import Data.Sequence (Seq)
-- import qualified Data.ByteString.Char8 as B
-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- import qualified Data.Text.Lazy.Encoding as T
-- import Data.Either (isRight)
-- import Data.Foldable

import Text.XML hiding (writeFile)
import Text.XML.Cursor

import Parse (getAttr, hasClass, hasStyle)

-- data Alignment = Unaligned | Centered deriving (Show)

data Format = FormatI | FormatB | FormatU | FormatS

instance Show Format where
  show FormatI = "I"
  show FormatB = "B"
  show FormatU = "U"
  show FormatS = "S"

data PostElement =
  PostImageGlobal Text
  | PostLink Text
  | PostImageLocal Text
  | PostCentered [PostElement]
  | PostLine Text
  | PostLineBreak
  | PostFormated Format [PostElement]
  | PostDice Text Text
  | PostSpoiler Text [PostElement]

postValidator :: (Bool, Bool, Bool) -> PostElement -> (Bool, Bool, Bool)
postValidator (bs, bi, bd) (PostFormated _ post) =
  let
    (cs, ci, cd) = isValidPost post
  in  
    (cs || bs, ci || bi, cd || bd)                            
postValidator (bs, bi, bd) (PostImageGlobal _)= (bs, True, bd) 
postValidator (bs, bi, bd) (PostCentered ps) =
  let
    (cs, ci, cd) = isValidPost ps 
  in  
    (cs || bs, ci || bi, cd || bd)
postValidator (bs, bi, bd) (PostDice _ _) = (bs, bi, True)
postValidator  (_, bi, bd) (PostSpoiler _ ps) =
  let
    (_, ci, cd) = isValidPost ps
  in
    (True, ci || bi, cd || bd)
postValidator v _ = v

isValidPost :: Post -> (Bool, Bool, Bool)
isValidPost = foldl postValidator (False, False, False)

instance Show PostElement where
  show (PostImageGlobal src) = "ImgGlobal[" ++ (T.unpack src) ++ "]" 
  show (PostImageLocal src) = "ImgLocal[" ++ (T.unpack src) ++ "]" 
  show (PostLine l) = T.unpack l ++ " "
  show PostLineBreak = "\n"
  show (PostCentered post) = "Сenter:\n" ++ (concatMap show post) ++ "CenterEnd\n" 
  show (PostSpoiler title body) = "Spoiler[" ++ T.unpack title ++ "]\n[" ++ (concatMap show body) ++ "]"
  show (PostDice value full) = "Dice[" ++ (T.unpack full) ++ ": " ++ (T.unpack value) ++ "]"
  show (PostFormated f post) = "Format" ++ show f ++ "[" ++ (concatMap show post)
  show (PostLink src) = "Link[" ++ (T.unpack src) ++ "]"

type Post = [PostElement]

savePost :: (MonadIO m) => String -> Post -> m ()
savePost prefix p =
  do
    let
      ps = concatMap show p
      nm = "./posts/" ++ prefix ++ ".txt"
    liftIO $ writeFile nm ps

extractPost :: Cursor -> Post
extractPost c = concatMap extractNode (child c)

extractNode :: Cursor -> Post
extractNode c =
  case node c of
    NodeContent text -> extractText text
    NodeElement el -> extractElement el c
    _ -> []

extractText :: Text -> Post
extractText txt =
  let
    trimmed = T.strip txt
  in
    if T.null trimmed
    then []
    else [PostLine trimmed]

extractElement :: Element -> Cursor -> Post
extractElement el c
  | tag == "a" = extractLink c
  | tag == "br" = [PostLineBreak]
  | tag == "i" = [PostFormated FormatI (extractPost c)]
  | tag == "b" = [PostFormated FormatB (extractPost c)]
  | tag == "u" = [PostFormated FormatU (extractPost c)]
  | tag == "s" = [PostFormated FormatS (extractPost c)]
  | tag == "img" = extractImage c
  | tag == "div" && hasClass "bbCodeSpoilerContainer" c = extractSpoiler c
  | tag == "div" && hasStyle "text-align: center" c = [PostCentered (extractPost c)]
  | tag == "div" && hasClass "dice_outer" c = extractDice c
  | otherwise = extractPost c
  where
    tag = nameLocalName (elementName el)

extractLink :: Cursor -> Post
extractLink c =
  case getAttr "href" c of
    Just src -> [PostLink src]
    Nothing ->  [PostLink "!Не удалось извлечь адрес ссылки!"]

extractDiceText :: Cursor -> Text
extractDiceText c =
  case c $// element "i" of
    (i : _) -> T.concat (i $// content)
    _ -> "!тэг i не найден!"

extractDiceValue :: Cursor -> Text
extractDiceValue c = T.concat (c $// element "span" >=> check (hasClass "dice_number") >=> child >=> content)

extractDice :: Cursor -> Post
extractDice c = [PostDice (extractDiceValue c) (extractDiceText c)]
    
extractImage :: Cursor -> Post
extractImage c =                 
    case getAttr "src" c of
      Just src ->
        if "http" `T.isPrefixOf` src
        then [PostImageGlobal src]
        else [PostImageLocal src]
      _ -> []

extractSpoiler :: Cursor -> Post
extractSpoiler c =
  let
    title = exractSpoilerTitle c
    cont = exractSpoilerContent c
  in
    [PostSpoiler title cont]

exractPageNumber :: Cursor -> Maybe Int
exractPageNumber c =
  let
    els = c $// element "div" &/ check (hasClass "PageNav")
  in
    case els of
      (e : _) ->
        do
          la <- getAttr "data-last" e
          readMaybe (T.unpack la)
      [] -> Nothing

exractSpoilerTitle :: Cursor -> Text
exractSpoilerTitle c =
  let
     spans = c $// element "span" &/ check (hasClass "SpoilerTitle") 
  in
    case spans of
      span : _ -> T.strip (T.concat (span $/ content))
      [] -> "Спойлер"
    
exractSpoilerContent :: Cursor -> Post
exractSpoilerContent c =
  let
    cs = c $// check (hasClass "bbCodeSpoilerText") 
  in
    case cs of
      c' : _ -> extractPost c'
      [] -> []
