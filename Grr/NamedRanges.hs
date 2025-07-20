{-# LANGUAGE OverloadedStrings #-}
module Grr.NamedRanges where
import qualified Data.Text as T
import Misc

replaceNamedRanges :: T.Text -> T.Text
replaceNamedRanges txt =
  T.concat $ map
  (
    \x ->
      case x of
        Left  l -> l
        Right r ->
          case named r of
            Nothing -> ("[:" `T.append` r `T.append` ":]")
            Just r' -> r'
  )
  (detectNamedRanges txt)

detectNamedRanges :: T.Text -> [Either T.Text T.Text]
detectNamedRanges T.Empty = []
detectNamedRanges ('[' T.:< ':' T.:< txt) =
  case back of
    (':' T.:< ']' T.:< rest) -> Right front:detectNamedRanges rest
    _                        -> Left  ('[' `T.cons` ':' `T.cons` front):detectNamedRanges back
  where (front, back) = splitEsc (== ':') txt
detectNamedRanges (x T.:< txt) = Left (x `T.cons` front):detectNamedRanges back
  where (front, back) = splitEsc (== '[') txt

-- Checks if it's a valid named range and return the range if it is.
named :: T.Text -> Maybe T.Text
named name =
  case name of
    "space" -> Just space
    "upper" -> Just upper
    "xdigit" -> Just xdigit
    "alnum" -> Just alnum
    "alpha" -> Just alpha
    "blank" -> Just blank
    "cntrl" -> Just cntrl
    "digit" -> Just digit
    "graph" -> Just graph
    "lower" -> Just lower
    "print" -> Just Grr.NamedRanges.print
    "punct" -> Just punct
    _ -> Nothing
-- Named ranges
space :: T.Text
space = T.pack [' ', '\t', '\n', '\v', '\f', '\r']
upper :: T.Text
upper = T.pack ['A'..'Z']
xdigit :: T.Text
xdigit = T.pack $ ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']
alnum :: T.Text
alnum = T.pack $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
alpha :: T.Text
alpha = T.pack $ ['a'..'z'] ++ ['A'..'Z']
blank :: T.Text
blank = T.pack [' ', '\t']
cntrl :: T.Text
cntrl = T.pack $ '\o177' :['\o00' ..'\o37']
digit :: T.Text
digit = T.pack ['0'..'9']
-- I'll keep it ASCII
graph :: T.Text
graph = T.pack ['!'..'~']
lower :: T.Text
lower = T.pack ['a'..'z']
print :: T.Text
print = T.pack [' '..'~']
punct :: T.Text
punct = T.pack ['#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/', '~', ':', ';', '<', '=', '>', '?', '@', '[', ']', '\\', '^', '_', '`', '|', '{', '}']
