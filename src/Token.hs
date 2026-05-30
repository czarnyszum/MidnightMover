
module Token where

import Data.List (isPrefixOf, find, break, dropWhile, isPrefixOf)
import Data.Char (digitToInt, isHexDigit, chr)
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Percent-decode a string (e.g. "%3C" -> '<')
urlDecode :: String -> String
urlDecode [] = []
urlDecode ('%':a:b:rest) | isHexDigit a && isHexDigit b =
    let byte = digitToInt a * 16 + digitToInt b
    in chr byte : urlDecode rest
urlDecode (c:rest) = c : urlDecode rest

-- Replace sequences R + two hex digits by % + the two digits, then percent-decode.
rDecode :: String -> String
rDecode = urlDecode . replaceR
  where
    replaceR [] = []
    replaceR ('R':a:b:rest)
      | isHexDigit a && isHexDigit b = '%' : a : b : replaceR rest
    replaceR (c:rest) = c : replaceR rest

-- Find first occurrence of a substring, return its index.
findSubStr :: String -> String -> Maybe Int
findSubStr needle haystack = go 0 haystack
  where
    go _ [] = Nothing
    go n h | needle `isPrefixOf` h = Just n
           | otherwise             = go (n+1) (tail h)

-- Extract a substring between `start` and `end` markers.
extractBetween :: String -> String -> String -> String
extractBetween start end s =
  let Just pos = findSubStr start s
      afterStart = drop (pos + length start) s
      Just endPos = findSubStr end afterStart
  in take endPos afterStart

-- Parse HTML to collect `id="..."` and the following text content until the next '<'.
extractIdContents :: String -> Map String String
extractIdContents html = Map.fromList $ go html
  where
    go [] = []
    go s =
      case findSubStr "id=\"" s of
        Nothing -> []
        Just pos ->
          let afterId = drop (pos + 4) s
              idName  = takeWhile (/= '"') afterId
              afterTag = drop 1 $ dropWhile (/= '>') afterId   -- skip past '>'
              (textContent, afterClose) = break (== '<') afterTag
              rest = dropWhile (== '<') afterClose
          in (idName, textContent) : go rest

-- Evaluate the concatenations using a map of element id -> innerHTML.
evaluateAssignments :: Map String String -> String -> String
evaluateAssignments idMap assignmentBlock =
  let stmts = filter (not . null) $ map trim $ splitOn ';' assignmentBlock
      env   = foldl (process idMap) Map.empty stmts
  in env Map.! "odXtiEO"
  where
    splitOn :: Eq a => a -> [a] -> [[a]]
    splitOn _ [] = []
    splitOn delim xs =
      let (before, after) = break (== delim) xs
      in before : case after of
                    []      -> []
                    (_:rest) -> splitOn delim rest

    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

    process idMap env stmt =
      case break (== '=') stmt of
        (var, '=':expr) ->
          let var' = trim var
              expr' = trim expr
              val   = if isKxkVZvGCU expr'
                      then idMap Map.! extractId expr'
                      else evalExpr env expr'
          in Map.insert var' val env
        _ -> env

    isKxkVZvGCU s = "KxkVZvGCU('" `isPrefixOf` s
    extractId s = takeWhile (/= '\'') $ drop 13 s   -- length of "KxkVZvGCU('"

    evalExpr env expr =
      let terms = map trim $ splitOn '+' expr
          values = map (evalTerm env) terms
      in concat values

    evalTerm env term
      | term == "''" = ""
      | ".innerHTML" `isSuffixOf` term =
          let varName = takeWhile (/= '.') term
          in Map.findWithDefault "" varName env
      | otherwise = Map.findWithDefault "" term env

-- Extract the value of an HTML attribute (e.g. name="8ab928bc").
extractAttrValue :: String -> String -> String
extractAttrValue attr html =
  let search = attr ++ "=\""
      Just pos = findSubStr search html
      start = pos + length search
      rest = drop start html
  in takeWhile (/= '"') rest

-- Main function: given the whole JavaScript code, return (name, value, formkeyToken).
extractTokens :: String -> (String, String, String)
extractTokens js =
  let -- initial huge encoded string for formkey
      initFormkeyRaw = extractBetween "unescape('" "'" js

      -- formetc hidden input encoded string
      formetcHTMLRaw = extractBetween "document.getElementById('formetc').innerHTML=unescape('" "'" js

      -- formetc value encoded string
      formetcValueRaw = extractBetween "document.getElementById('formetc').getElementsByTagName('*')[0].value=unescape('" "'" js

      -- decode initial formkey HTML and build id -> innerHTML map
      initialHTML = rDecode initFormkeyRaw
      idMap       = extractIdContents initialHTML

      -- extract assignment block between first and last formkey setting
      startAssign = findSubStr "'.replace(/R([\\w]{2})/g,'%$1'));" js  -- end of first unescape
      Just assignStartPos = startAssign
      afterFirst = drop (assignStartPos + length "'.replace(/R([\\w]{2})/g,'%$1'));") js
      finalMarker = "document.getElementById('formkey').innerHTML=unescape(odXtiEO.replace(/R([\\w]{2})/g,'%$1'));"
      Just finalPos = findSubStr finalMarker afterFirst
      assignBlock = take finalPos afterFirst

      -- compute odXtiEO and final token
      odXtiEO = evaluateAssignments idMap assignBlock
      finalToken = rDecode odXtiEO

      -- decode hidden input name and value
      decodedFormetcHTML = rDecode formetcHTMLRaw
      hiddenName  = extractAttrValue "name" decodedFormetcHTML
      hiddenValue = rDecode formetcValueRaw

  in (hiddenName, hiddenValue, finalToken)

tokenTest :: MonadIO m => m (String, String, String)
tokenTest =
  do
    js <- liftIO $ readFile "test.js"
    r <- extractTokens js
    liftIO $ print r
    return r
