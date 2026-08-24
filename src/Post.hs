{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Post (Post, isValidPost, extractPost, savePost, exractPageNumber, toBBCMap) where

import Control.Monad
import Control.Monad.IO.Class

import Data.Char (isControl)
import Data.List (find)
import Data.Maybe (isJust, fromJust)

import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

import System.Directory (createDirectoryIfMissing)


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
  | PostTable [[Post]]
  | PostSize Int [PostElement]
  | PostFont Text [PostElement]

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
  show (PostTable rows) = "Table[" ++ concatMap (concatMap (concatMap show)) rows ++ "]"
  show (PostSize n post) = "Size" ++ show n ++ "[" ++ concatMap show post ++ "]"
  show (PostFont f post) = "Font[" ++ T.unpack f ++ " " ++ concatMap show post ++ "]"

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
toBBC (PostColor col post) = let x = toBBCMap post in T.concat ["[color=", col, "]", x, "[/color]"]
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
toBBC (PostTable rows) =
  let
    row r = T.concat ["[tr]", T.concat (map cell r), "[/tr]"]
    cell p = T.concat ["[td]", toBBCMap p, "[/td]"]
  in
    T.concat ["[table]", T.concat (map row rows), "[/table]"]
toBBC (PostSize n post) = let x = toBBCMap post in T.concat ["[size=", T.pack (show n), "]", x, "[/size]"]
toBBC (PostFont f post) = let x = toBBCMap post in T.concat ["[font=", f, "]", x, "[/font]"]
   
 
type Post = [PostElement]

-- | Replace characters that are unsafe / unsupported in file names.
sanitizeFileName :: String -> String
sanitizeFileName = map sanitize
  where
    sanitize c
      | c `elem` ("/\\:*?\"<>|" :: String) || isControl c = '_'
      | otherwise = c

savePost :: (MonadIO m) => String -> Post -> m String
savePost prefix p =
  do
    let
      ps = T.unpack . T.strip . toBBCMap $ p
      dir = "./posts"
      nm = dir ++ "/" ++ sanitizeFileName prefix ++ ".txt"
    liftIO $ createDirectoryIfMissing True dir
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
    trimmed = T.strip txt -- skip whitespace-only text nodes
  in
    if T.null trimmed
    then []
    else [PostLine txt]

hasColor :: Cursor -> Maybe Text
hasColor c = do
  style <- getAttr "style" c
  let
    decls = map T.strip (T.splitOn ";" style)
  colorDecl <- find ("color:" `T.isPrefixOf`) decls
  let
    colorValue = T.strip . T.drop (T.length "color:") $ colorDecl
  guard $ not $ T.null colorValue
  return colorValue

-- | "font-size: 18px" -> Just 18 (the bunker renders [size=N] as N px).
hasFontSize :: Cursor -> Maybe Int
hasFontSize c = do
  style <- getAttr "style" c
  let
    decls = map T.strip (T.splitOn ";" style)
  sizeDecl <- find ("font-size:" `T.isPrefixOf`) decls
  let
    sizeValue = T.strip . T.drop (T.length "font-size:") $ sizeDecl
    digits = T.takeWhile (\ch -> ch >= '0' && ch <= '9') sizeValue
  guard $ not $ T.null digits
  readMaybe (T.unpack digits)

-- | "font-family: Verdana" (quotes stripped) -> Just "Verdana".
hasFontFamily :: Cursor -> Maybe Text
hasFontFamily c = do
  style <- getAttr "style" c
  let
    decls = map T.strip (T.splitOn ";" style)
  fontDecl <- find ("font-family:" `T.isPrefixOf`) decls
  let
    fontValue = T.strip . T.drop (T.length "font-family:") $ fontDecl
    stripped = T.filter (\ch -> ch /= '\'' && ch /= '"') fontValue
  guard $ not $ T.null stripped
  return stripped

-- | XF2 tables: <table><tr><td>…</td>…</tr></table> (cells may contain
--   formatted content). Renders as [table][tr][td]…[/td][/tr][/table],
--   which the target forum understands.
extractTable :: Cursor -> Post
extractTable c =
  let
    rows = c $// element "tr"
  in
    [PostTable (map extractTableRow rows)]

extractTableRow :: Cursor -> [Post]
extractTableRow r =
  let
    tds = r $/ element "td"
    ths = r $/ element "th"
  in
    map extractPost (if null tds then ths else tds)

extractElement :: Element -> Cursor -> Post
extractElement el c                      
  | tag == "script" = []
  | tag == "iframe" = extractYouTube c 
  | tag == "a" = extractLink c 
  | tag == "br" = [PostLineBreak]
  | tag == "i" = [PostFormated FormatI (extractPost c)]
  | tag == "b" = [PostFormated FormatB (extractPost c)]
  | tag == "u" = [PostFormated FormatU (extractPost c)]
  | tag == "s" = [PostFormated FormatS (extractPost c)]
  | tag == "span" && hasStyle "text-decoration: line-through" c = [PostFormated FormatS (extractPost c)]
  | tag == "span" && isJust (hasColor c) = [PostColor (fromJust $ hasColor c) (extractPost c) ] 
  | tag == "span" && isJust (hasFontSize c) = [PostSize (fromJust $ hasFontSize c) (extractPost c)]
  | tag == "span" && isJust (hasFontFamily c) = [PostFont (fromJust $ hasFontFamily c) (extractPost c)]
  | tag == "img" = extractImage c
  | tag == "table" = extractTable c
  | tag == "blockquote" && hasClass "bbCodeBlock--quote" c = extractQuote c
  | tag == "div" && hasClass "bbCodeQuote" c = extractQuote c -- XF1 legacy
  | tag == "div" && hasClass "quoteExpand" c = []
  | tag == "div" && hasClass "bbCodeSpoiler" c = extractSpoiler c
  | tag == "div" && hasClass "bbCodeSpoilerContainer" c = extractSpoiler c -- XF1 legacy
  | tag == "div" && hasClass "bbCodeBlock-expandLink" c = []
  | tag == "div" && hasClass "js-selectToQuoteEnd" c = []
  | (tag == "div" || tag == "p") && hasStyle "text-align: center" c = [PostCentered (extractPost c)]
  | tag == "div" && hasClass "dice_outer" c = extractDice c
  | otherwise = extractPost c
  where
    tag = nameLocalName (elementName el)

extractQuote :: Cursor -> Post
extractQuote c =
  case getAttr "data-quote" c of
    Just author -> quoteFrom author
    Nothing ->
      case getAttr "data-author" c of -- XF1 legacy (div.bbCodeQuote)
        Just author -> quoteFrom author
        Nothing -> []
  where
    -- XF2: content lives in div.bbCodeBlock-expandContent (inside
    -- div.bbCodeBlock-content); fall back to div.quote for the old XF1 markup.
    quoteFrom author =
      let
        cs = c $// element "div" >=> check (hasClass "bbCodeBlock-expandContent")
        cs' = case cs of
                (q : _) -> [q]
                [] -> c $// element "div" >=> check (hasClass "bbCodeBlock-content")
        cs'' = case cs' of
                 (q : _) -> [q]
                 [] -> c $// element "div" >=> check (hasClass "quote")
      in
        case cs'' of
          (q : _) -> [PostQuote author (extractPost q)]
          [] -> []

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
    
-- | Make a relative src absolute against the source forum.
absolutizeUrl :: Text -> Text
absolutizeUrl src
  | "http" `T.isPrefixOf` src = src
  | "/" `T.isPrefixOf` src = T.concat ["https://simsmix.ru", src]
  | otherwise = src

extractImage :: Cursor -> Post
extractImage c
  -- Smilies: the target forum has a different smiley set, so emit the smiley
  -- image URL instead of the text code (which would not render there).
  | hasClass "smilie" c =
      case getAttr "src" c of
        Just src -> [PostImageGlobal (absolutizeUrl src)]
        Nothing -> []
  | otherwise =
      let
        -- XF2 wraps images in div.bbImageWrapper > img.bbImage; the img carries
        -- the real URL in data-url (proxy.php URLs have a relative src but a
        -- full data-url).
        mUrl =
          case getAttr "data-url" c of
            Just u | "http" `T.isPrefixOf` u -> Just u
            _ -> getAttr "src" c
      in
        case mUrl of
          Just src
            | "http" `T.isPrefixOf` src -> [PostImageGlobal src]
            | otherwise ->
                case getAttr "alt" c of
                  Just ty -> [PostImageLocal ty]
                  Nothing -> [PostImageLocal src]
          Nothing -> []

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
    -- XF2: the page-jump input carries the last page in its max attribute
    -- (old XF1 markup had div.PageNav with data-last)
    els = c $// element "input" >=> check (hasClass "js-pageJumpPage")
  in
    case els of
      (e : _) ->
        do
          mx <- getAttr "max" e
          readMaybe (T.unpack mx)
      [] -> Nothing

exractSpoilerTitle :: Cursor -> Text
exractSpoilerTitle c =
  let
    spans = c $// element "span" >=> check (hasClass "bbCodeSpoiler-button-title")
  in
    case spans of
      span : _ -> T.strip (T.concat (span $/ content))
      [] -> "Спойлер"

exractSpoilerContent :: Cursor -> Post
exractSpoilerContent c =
  let
    cs = c $// check (hasClass "bbCodeSpoiler-content")
  in
    case cs of
      c' : _ -> extractPost c'
      [] -> []
