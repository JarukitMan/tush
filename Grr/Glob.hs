{-# LANGUAGE OverloadedStrings #-}
module Grr.Glob(glob) where
import qualified Data.Text as T
import Grr.NamedRanges
import Misc
import Data.Maybe

-- TODO: Named ranges. Shouldn't be too hard.

data GlobPrimitives = String T.Text | Any | Star | List Bool T.Text
  deriving (Show)
type Glob = [GlobPrimitives]

glob :: T.Text -> T.Text -> Bool
glob pat = match (txt2glb pat)

txt2glb :: T.Text -> Glob
txt2glb T.Empty = []
txt2glb ('[' T.:< x T.:< txt) =
  -- Since this uses a xor,
  -- | Orig | Side | Out
  -- |  T   |  T   | F
  -- |  T   |  F   | T
  -- |  F   |  T   | T
  -- |  F   |  F   | F
  -- Therefore False is inside and True is outside.
  if x == '!'
  then List True front:txt2glb rest
  else List False (x T.:< front):txt2glb rest
  where
    -- If it's not closed... Then it's just not a range.
    (f, back) = fromMaybe ("[", txt) $ splitInside 1 '[' txt
    front = replaceNamedRanges f
    rest = T.drop 1 back
txt2glb ('?' T.:< txt) = Any:txt2glb txt
txt2glb ('*' T.:< txt) = Star:txt2glb txt
txt2glb txt =
  String front:txt2glb back
  where (front, back) = splitEsc (`T.elem` "[*?") txt

match :: Glob -> T.Text -> Bool
match (String x:xs) txt =
  case x `T.stripPrefix` txt of
    Nothing -> False
    Just ys -> match xs ys
match (Any:xs) (_ T.:< txt) = match xs txt
match (Star:xs) txt =
  let
    (front, back) = splitWith (\g -> case g of {String _ -> True; _ -> False}) xs
    questions = length (filter (\g -> case g of {Any -> True; _ -> False}) front)
    txt' = T.drop (fromIntegral questions) txt
  in
    case back of
      (String str:rest) ->
        case str `searchText` txt' of
          [] -> False
          indices ->
            or
            (
              map
              (
                \i ->
                  match rest $
                  (
                    T.drop
                    (fromIntegral i + T.length str)
                  )
                  txt'
              )
              indices
            )
      _ -> if T.length txt > (fromIntegral questions) then True else False
match (List side chars:xs) (t T.:< txt) =
  -- /= works as a xor.
  side /=
  (t `T.elem` chars) &&
  match xs txt
match [] (T.Empty) = True
match _ _ = False
