module Grr(Grr.filter, match, score, order) where

import qualified Data.Text as T
import Grr.Glob
import Grr.Regex
import Data.List(sortOn)
import Data.Char(toLower)
import Data.Array(array, (!), (//), Array)

data Pattern = RegEx | Glob
type CaseSensitive = Bool

match :: Pattern -> CaseSensitive -> T.Text -> T.Text -> Bool
match style cs pattern text =
  (
    case style of
      Glob -> glob
      RegEx -> regex
  )
  (if cs then pattern else lowerEsc pattern)
  (if cs then text else lowerEsc text)

filter :: Pattern -> CaseSensitive -> T.Text -> [T.Text] -> [T.Text]
filter pattern cs needle haystack = Prelude.filter (match pattern cs needle) haystack

lowerEsc :: T.Text -> T.Text
lowerEsc ('\\' T.:< x T.:< xs) = '\\' `T.cons` (x `T.cons` lowerEsc xs)
lowerEsc (x T.:< xs) = toLower x `T.cons` lowerEsc xs
lowerEsc (T.Empty) = T.empty

score :: T.Text -> T.Text -> Int
score a b = fst $ scoreInternal (la, a) (lb, b) (array ((1, 1), (la, lb)) [((x, y), -1) | x <- [1..la], y <- [1..lb]])
  where (la, lb) = (T.length a, T.length b)
scoreInternal :: (Int, T.Text) -> (Int, T.Text) -> Array (Int, Int) Int -> (Int, Array (Int, Int) Int)
scoreInternal (la, a) (lb, b) table
  |
  la == 0 && lb == 0 = (0, table)
  |
  la == 0 = (lb, table)
  |
  lb == 0 = (la, table)
  |
  otherwise =
    case table ! (la, lb) of
      -1 ->
        case (T.uncons a, T.uncons b) of
          (Nothing, Nothing) -> (0, table)
          (Nothing, Just _) -> (lb, table)
          (Just _, Nothing) -> (la, table)
          (Just (ha, as), Just (hb, bs)) ->
            let
              d = if ha == hb then 0 else 1
              (n1, t1) = scoreInternal (pred la, as) (pred lb, bs) table
              (n2, t2) = scoreInternal (la, a) (pred lb, bs) t1
              (n3, t3) = scoreInternal (pred la, as) (lb, b) t2
              x = d + minimum [n1, n2, n3]
            in
              (x, t3 // [((la, lb), x)])
      x -> (x, table)

order :: T.Text -> [T.Text] -> [T.Text]
order txt = sortOn (score txt)
