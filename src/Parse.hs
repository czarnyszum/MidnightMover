{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Parse (extractMessages, Message) where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Sequence (Seq)
import qualified Data.Sequence as S

import Text.HTML.TagSoup

import Debug.Trace

-- [(ByteString, ByteString, [Tag ByteString])]   -- ^ (id, author, содержимое)

type Message = (ByteString, ByteString, Seq (Tag ByteString))

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
        trace ((show $ (msgId, author)) ++ "\n") $ return (msgId, author)  
    else Nothing


hasMessageListClass :: [Attribute ByteString] -> Bool
hasMessageListClass attrs = any (\(k, v) -> k == "class" && "messageList" `elem` B.words (BL.toStrict v)) attrs

transMessageExtractor :: MessageExtractor -> Tag ByteString -> MessageExtractor
transMessageExtractor LookingForMessageList (TagOpen "ol" attrs) =
  if hasMessageListClass attrs
  then trace "MessageList start" $ LookingForLiStart 0 S.empty
  else LookingForMessageList
transMessageExtractor LookingForMessageList _ = LookingForMessageList

transMessageExtractor (LookingForLiStart n ts) (TagOpen "ol" _) = trace ("open ol" ++ show n) $ LookingForLiStart (n + 1) ts
transMessageExtractor (LookingForLiStart 0 ts) (TagClose "ol")  = trace ("Stop") $ Stop ts
transMessageExtractor (LookingForLiStart n ts) (TagClose "ol")  = trace ("close ol" ++ show n) $ LookingForLiStart (n - 1) ts

transMessageExtractor (LookingForLiStart n ts) (TagOpen "li" attrs) =
  case isMessage attrs of
    Just (msgId, author) -> LookingForLiEnd n ts (msgId, author, S.empty)
    Nothing -> LookingForLiStart n ts
transMessageExtractor (LookingForLiStart n ts) _ = LookingForLiStart n ts

transMessageExtractor (LookingForLiEnd n ts t) (TagOpen "ol" _) = trace ("open ol" ++ show n) $ LookingForLiEnd (n + 1) ts t
transMessageExtractor (LookingForLiEnd n ts t) (TagClose "ol") = trace ("close ol" ++ show n) $ LookingForLiEnd (n - 1) ts t

transMessageExtractor (LookingForLiEnd n ts t) (TagClose "ol") = LookingForLiEnd (n - 1) ts t
transMessageExtractor (LookingForLiEnd n ts t) (TagClose "li") = LookingForLiStart n (ts S.|> t)
transMessageExtractor (LookingForLiEnd n ts (m, a, xs)) x = LookingForLiEnd n ts (m, a, xs S.|> x)
transMessageExtractor (Stop ts) _ = Stop ts

extractMessages
  :: [Tag ByteString]                               -- ^ Тэги внутри messageList
  -> Maybe (Seq Message)   -- ^ (id, author, содержимое)
extractMessages tags =
  let
    state = foldl' transMessageExtractor LookingForMessageList tags
  in
    case state of
      Stop ts -> Just ts
      _ -> Nothing
  
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
