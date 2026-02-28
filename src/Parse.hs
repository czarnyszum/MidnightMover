
module Parse (extractMessages) where

import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)


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
  where
    isOlOpen (TagOpen "ol" _) = True
    isOlOpen _ = False

    isOlClose (TagClose "ol") = True
    isOlClose _               = False

    hasMessageListClass attrs =
      any (\(k, v) -> k == "class"
        && "messageList" `elem` B.words (BL.toStrict v)) attrs
    
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
  where
    isLiClose (TagClose "li") = True
    isLiClose _ = False

    hasClassMessage attrs =
      case lookup "class" attrs of
        Just cls ->
          -- ищем "message" как отдельное слово внутри class-строки
          -- допускаем пробел в начале/конце (обычно в HTML бывает "message " или " message")
           "message" `elem` B.words (BL.toStrict cls)
        Nothing -> False
