{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Parse (extractMessages, Message, getAttr) where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Sequence (Seq)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

-- import qualified Data.Sequence as S

import Text.XML
import Text.XML.Cursor

import Debug.Trace

-- [(ByteString, ByteString, [Tag ByteString])]   -- ^ (id, author, содержимое)

type Message = (Text, Text, Cursor)

attrIs :: Text -> Text -> Cursor -> [Cursor]
attrIs attr val c = [c | Just val == getAttr (Name attr Nothing Nothing) c]

isMessage :: Cursor -> [Message]
isMessage c =
  case (getAttr "id" c, getAttr "data-author" c) of
    (Just msgId, Just msgAuthor) -> [(msgId, msgAuthor, c)] -- | "message " `elem` T.words cls
    (_, _) -> []

getAttr :: Name -> Cursor -> Maybe Text
getAttr attrName c =
  case node c of
    NodeElement el -> M.lookup attrName (elementAttributes el)
    _ -> Nothing

extractMessages :: Cursor -> [Message]
extractMessages cursor =
  let
    messageList = cursor $// element "ol" >=> (attrIs "id" "messageList")
    messageCursors = messageList >>= child >>= element "li" 
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
