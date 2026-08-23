{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Post (Post, isValidPost, extractPost, savePost, exractPageNumber, toBBCMap) where

import Control.Monad
import Control.Monad.IO.Class

import Data.Maybe (listToMaybe, isJust, fromJust)

import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)


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
  | PostLink Text Text
  | PostYouTube Text
  | PostImageLocal Text
  | PostColor Text [PostElement]
  | PostCentered [PostElement]
  | PostQuote Text [PostElement]
  | PostLine Text
  | PostLineBreak
  | PostFormated Format [PostElement]
  | PostDice Text Text
  | PostSpoiler Text [PostElement]

isFormated :: PostElement -> Bool
isFormated (PostFormated _ _) = True
isFormated _ = False                      

isLine :: PostElement -> Bool
isLine (PostLine t) = not $ T.null t
isLine _ = False                      


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
  show (PostColor col post) = "Colored[" ++ (T.unpack col) ++ " " ++ (concatMap show post) ++ "]"
  show (PostLine l) = T.unpack l ++ " "
  show PostLineBreak = "\n"
  show (PostQuote author post) = "Quote[" ++ (T.unpack author) ++ ": " ++ (concatMap show post) ++ "]"
  show (PostCentered post) = "Сenter:\n" ++ (concatMap show post) ++ "CenterEnd\n" 
  show (PostSpoiler title body) = "Spoiler[" ++ T.unpack title ++ "]\n[" ++ (concatMap show body) ++ "]"
  show (PostDice value full) = "Dice[" ++ (T.unpack full) ++ ": " ++ (T.unpack value) ++ "]"
  show (PostFormated f post) = "Format" ++ show f ++ "[" ++ (concatMap show post) ++ "]"
  show (PostLink src text) = "Link[" ++  (T.unpack text) ++ ": " ++ (T.unpack src) ++ "]"
  show (PostYouTube src)= "YouTube[" ++ (T.unpack src) ++ "]"

{-
[color=black][/color]
[color=silver][/color]
[color=gray][/color]
[color=white][/color]
[color=maroon][/color]
[color=red][/color]
[color=purple][/color]
[color=fuchsia][/color]
[color=green][/color]
[color=lime][/color]
[color=olive][/color]
[color=yellow][/color]
[color=navy][/color]
[color=blue][/color]
[color=teal][/color]
[color=aqua][/color]
#xxyyzz
-}

toBBCMap :: Post -> Text
toBBCMap ps = T.concat $ map toBBC ps
toBBC :: PostElement -> Text
toBBC (PostImageGlobal src) = T.concat ["[img]", src, "[/img]"] 
toBBC (PostImageLocal src) = src
toBBC (PostColor col post) = let x = toBBCMap post in T.concat ["[color=", col, "]", x, "[/img]"]
toBBC (PostLine l) = l
toBBC PostLineBreak = "\n"
toBBC (PostQuote author post) = let x = toBBCMap post in T.concat ["[quote=", author, "]", x, "[/quote]"] 
toBBC (PostCentered post) = let x = toBBCMap post in T.concat ["[align=center]", x, "[/align]"]
toBBC (PostSpoiler title body) = let x = toBBCMap body in T.concat [ "[spoiler=", title, "]", x, "[/spoiler]"]
toBBC (PostDice value full) = T.concat [full, ": ", value]
toBBC (PostFormated FormatI post) = let x = toBBCMap post in T.concat ["[i]", x, "[/i]"]
toBBC (PostFormated FormatB post) = let x = toBBCMap post in T.concat ["[b]", x, "[/b]"]
toBBC (PostFormated FormatU post) = let x = toBBCMap post in T.concat ["[u]", x, "[/u]"]
toBBC (PostFormated FormatS post) = let x = toBBCMap post in T.concat ["[s]", x, "[/s]"]
toBBC (PostLink src text) = T.concat ["[url=", src, "]", text, "[/url]"]
toBBC (PostYouTube src) = T.concat ["[video]", src, "[/video]"]
   
 
type Post = [PostElement]

savePost :: (MonadIO m) => String -> Post -> m String
savePost prefix p =
  do
    let
      ps = T.unpack . toBBCMap $ p
      nm = "./posts/" ++ prefix ++ ".txt"
    liftIO $ writeFile nm ps
    return nm

{-
savePost :: (MonadIO m) => String -> Post -> m ()
savePost prefix p =
  do
    let
      ps = concatMap show p
      nm = "./posts/" ++ prefix ++ ".txt"
    liftIO $ writeFile nm ps
-}



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
    trimmed = txt -- T.strip txt
  in
    if T.null trimmed
    then []
    else [PostLine trimmed]

hasColor :: Cursor -> Maybe Text
hasColor c =
  do
    style <- getAttr "style" c
    guard $ T.isPrefixOf "color:" style
    let
      colorValue = T.strip . T.drop (T.length "color:") $ style
    guard $ not $ T.null colorValue
    return colorValue

extractElement :: Element -> Cursor -> Post
extractElement el c                      
  | tag == "iframe" = extractYouTube c 
  | tag == "a" = extractLink c 
  | tag == "br" = [PostLineBreak]
  | tag == "i" = [PostFormated FormatI (extractPost c)]
  | tag == "b" = [PostFormated FormatB (extractPost c)]
  | tag == "u" = [PostFormated FormatU (extractPost c)]
  | tag == "span" && hasStyle "text-decoration: line-through" c = [PostFormated FormatS (extractPost c)]
  | tag == "span" && isJust (hasColor c) = [PostColor (fromJust $ hasColor c) (extractPost c) ] 
  | tag == "img" = extractImage c
  | tag == "div" && hasClass "bbCodeQuote" c = extractQuote c
  | tag == "div" && hasClass "quoteExpand" c = []
  | tag == "div" && hasClass "bbCodeSpoilerContainer" c = extractSpoiler c
  | tag == "div" && hasStyle "text-align: center" c = [PostCentered (extractPost c)]
  | tag == "div" && hasClass "dice_outer" c = extractDice c
  | otherwise = extractPost c
  where
    tag = nameLocalName (elementName el)

extractQuote :: Cursor -> Post
extractQuote c =
  case getAttr "data-author" c of
    Just author ->
      case c $// element "div" >=> check (hasClass "quote") of
        (q : _) -> [PostQuote author . extractPost $ q]
        [] -> []
    Nothing -> []

extractLink :: Cursor -> Post
extractLink c =
  let
    text = T.concat (c $/ content)
  in
    case getAttr "href" c of
      Just src -> [PostLink src text]
      Nothing ->  [PostLink "!Не удалось извлечь адрес ссылки!" text]

extractYouTube :: Cursor -> Post
extractYouTube c =
    case getAttr "src" c of
      Just src -> [PostYouTube src]
      Nothing ->  [PostYouTube "!Не удалось извлечь адрес ссылки!"]

extractDiceText :: Cursor -> Text
extractDiceText c =
  case c $// element "i" of
    (i0 : i1 : []) -> T.concat [T.concat (i0 $// content), T.concat (i1 $// content)]
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
        else
          case getAttr "alt" c of
            Just ty -> [PostImageLocal ty]
            Nothing -> [PostImageLocal src]
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
