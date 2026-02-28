{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Parse (extractMessageList, extractMessages) where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Sequence (Seq)
import qualified Data.Sequence as S

import Text.HTML.TagSoup

-- [(ByteString, ByteString, [Tag ByteString])]   -- ^ (id, author, содержимое)

type Message = (ByteString, ByteString, [Tag ByteString])

data MessageExtractor =
    LookingForMessageList
  | LookingForLiStart Int (Seq Message)
  | LookingForLiEnd Int (Seq Message) Message
  | Stop (Seq Message)

isOlOpen :: Tag ByteString -> Bool
isOlOpen (TagOpen "ol" _) = True
isOlOpen _ = False

isOlClose :: Tag ByteString -> Bool
isOlClose (TagClose "ol") = True
isOlClose _               = False

isLiClose :: Tag ByteString -> Bool
isLiClose (TagClose "li") = True
isLiClose _ = False

hasClassMessage :: [Attribute ByteString] -> Bool
hasClassMessage attrs =
  case lookup "class" attrs of
    Just cls ->
      -- ищем "message" как отдельное слово внутри class-строки
      -- допускаем пробел в начале/конце (обычно в HTML бывает "message " или " message")
      "message" `elem` B.words (BL.toStrict cls)
    Nothing -> False

hasMessageListClass :: [Attribute ByteString] -> Bool
hasMessageListClass attrs = any (\(k, v) -> k == "class" && "messageList" `elem` B.words (BL.toStrict v)) attrs

transMessageExtractor :: Tag ByteString -> MessageExtractor -> MessageExtractor
transMessageExtractor (TagOpen "ol" _) LookingForMessageList = LookingForLiStart 0 S.empty
transMessageExtractor _ LookingForMessageList = LookingForMessageList

transMessageExtractor (TagOpen "ol" _)  (LookingForLiStart n ts) = LookingForLiStart (n + 1) ts
transMessageExtractor (TagClose "ol" _) (LookingForLiStart 0 ts) = Stop ts
transMessageExtractor (TagClose "ol" _) (LookingForLiStart n ts) = LookingForLiStart (n - 1) ts
transMessageExtractor (TagOpen "li" _)  (LookingForLiStart n ts) = LookingForLiEnd n ts
transMessageExtractor t (LookingForLiStart n ts) = LookingForLiStart n (ts S.<| t)

transMessageExtractor (TagClose "ol" _) (LookingForEnd n ts) = LookingForLiEnd (n - 1) ts
transMessageExtractor (TagClose "li" _)  (LookingForLiEnd n ts)  = LookingForLiStart n ts

transMessageExtractor _ (Stop ts) = Stop ts


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

