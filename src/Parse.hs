{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Parse (extractMessages, Message, getAttr, hasClass, hasStyle, attrIs) where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

import Text.XML
import Text.XML.Cursor

-- [(ByteString, ByteString, [Tag ByteString])]   -- ^ (id, author, содержимое)

-- | (postId, author, cursor on the message body).
--   postId   - "post-282681" (from data-content)
--   author   - display name or numeric user id (from data-author)
--   cursor   - on <article class="message-body js-selectToQuote">, i.e. the
--              actual post content (BBCode source of the message)
type Message = (Text, Text, Cursor)

attrIs :: Text -> Text -> Cursor -> [Cursor]
attrIs attr val c = [c | Just val == getAttr (Name attr Nothing Nothing) c]

getAttr :: Name -> Cursor -> Maybe Text
getAttr attrName c =
  case node c of
    NodeElement el -> M.lookup attrName (elementAttributes el)
    _ -> Nothing

hasStyle :: Text -> Cursor -> Bool
hasStyle st c =
  case getAttr "style" c of
    Just styles -> st `elem` map T.strip (T.splitOn ";" styles)
    Nothing -> False

hasClass :: Text -> Cursor -> Bool
hasClass cls c =
  case getAttr "class" c of
    Just classes -> cls `elem` T.words classes
    Nothing -> False

-- | Message id: XenForo 2 puts it into data-content ("post-282681");
--   fall back to id="js-post-282681" (strip the "js-post-" prefix).
msgIdOf :: Cursor -> Maybe Text
msgIdOf c =
  case getAttr "data-content" c of
    Just v | not (T.null v) -> Just v
    _ ->
      case getAttr "id" c of
        Just v -> T.stripPrefix "js-post-" v
        Nothing -> Nothing

isMessage :: Cursor -> [Message]
isMessage c =
  case (msgIdOf c, getAttr "data-author" c) of
    (Just msgId, Just msgAuthor) ->
      let
        cs = c $// element "article" >=> check (hasClass "message-body")
      in
        case cs of
          c' : _ -> [(msgId, msgAuthor, c')]
          [] -> []
    (_, _) -> []

extractMessages :: Cursor -> [Message]
extractMessages cursor =
  let
    -- XenForo 2.x: messages are <article class="message ... message--post js-post">
    -- (the old XF1 markup had <ol id="messageList"> with <li> children)
    messageCursors = cursor $// element "article" >=> check (hasClass "message--post")
  in
    concatMap isMessage messageCursors 

{-

data MessageExtractor =
    LookingForMessageList
  | LookingForLiStart Int (Seq Message)
  | LookingForLiEnd Int (Seq Message) Message
  | Stop (Seq Message)

isMessage :: [Attribute ByteString] -> Maybe (ByteString, ByteString)
isMessage attrs =
  do
    cls <- lookup "class" attrs
    if "message" `elem` B.words (BL.toStrict cls)
    then
      do
        msgId <- lookup "id" attrs
        author <- lookup "data-author" attrs
        return (msgId, author)  
    else Nothing


hasMessageListClass :: [Attribute ByteString] -> Bool
hasMessageListClass attrs = any (\(k, v) -> k == "class" && "messageList" `elem` B.words (BL.toStrict v)) attrs

trans :: MessageExtractor -> Tag ByteString -> MessageExtractor
trans LookingForMessageList (TagOpen "ol" attrs) =
  if hasMessageListClass attrs
  then LookingForLiStart 0 S.empty
  else LookingForMessageList
trans LookingForMessageList _ = LookingForMessageList

trans (LookingForLiStart n ts) (TagOpen "ol" _) = LookingForLiStart (n + 1) ts
trans (LookingForLiStart 0 ts) (TagClose "ol")  = Stop ts
trans (LookingForLiStart n ts) (TagClose "ol")  = LookingForLiStart (n - 1) ts

trans (LookingForLiStart n ts) (TagOpen "li" attrs) =
  case isMessage attrs of
    Just (msgId, author) -> LookingForLiEnd n ts (msgId, author, S.empty)
    Nothing -> LookingForLiStart n ts
trans (LookingForLiStart n ts) _ = LookingForLiStart n ts

trans (LookingForLiEnd n ts t) (TagOpen "ol" _) = LookingForLiEnd (n + 1) ts t
trans (LookingForLiEnd n ts t) (TagClose "ol") = LookingForLiEnd (n - 1) ts t

trans (LookingForLiEnd n ts t) (TagClose "ol") = LookingForLiEnd (n - 1) ts t
trans (LookingForLiEnd n ts t) (TagClose "li") = LookingForLiStart n (ts S.|> t)
trans (LookingForLiEnd n ts (m, a, xs)) x = LookingForLiEnd n ts (m, a, xs S.|> x)
trans (Stop ts) _ = Stop ts

-}
  
{-
-- should ignore inner ols
-- Просто найди первый <ol class="messageList">
extractMessageList :: [Tag ByteString] -> Maybe [Tag ByteString]
extractMessageList tags =
  case break isOlOpen tags of
    (_, TagOpen "ol" attrs : rest) ->
      if hasMessageListClass attrs
      then Just (takeWhile (not . isOlClose) rest)
      else extractMessageList rest
    _ -> Nothing

    
-- | Извлечь сообщения в формате (id, author, innerHtml) из блока messageList
extractMessages
  :: [Tag ByteString]                -- ^ Тэги внутри messageList
  -> [(ByteString, ByteString, [Tag ByteString])]   -- ^ (id, author, содержимое)
extractMessages [] = []
extractMessages (tag:rest) =
  case tag of
    TagOpen "li" attrs
      | hasClassMessage attrs
      , Just msgId <- lookup "id" attrs
      , Just author <- lookup "data-author" attrs
        ->
          let
            (inner, after) = break (isLiClose) rest
          in
            (msgId, author, inner) : extractMessages (drop 1 after)
    _ -> extractMessages rest

-}
