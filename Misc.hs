{-# LANGUAGE OverloadedStrings #-}
module Misc where
-- import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

allRight :: [Either a b] -> Either a [b]
allRight [] = Right []
allRight (x:xs) =
      case x of
        Left err -> Left err
        Right x' ->
          case allRight xs of
            Left err  -> Left err
            Right xs' -> Right (x':xs')

dupes :: (Eq a) => [a] -> Bool
dupes [] = True
dupes [_] = True
dupes (x1:x2:xs) = x1 == x2 && dupes (x2:xs)

isInt :: String -> Bool
isInt [] = False
isInt (x:xs) =
  if x `elem` ('-':['0'..'9'])
  then and (map (`elem` ['0'..'9']) xs)
  else False

intMaybe :: T.Text -> Maybe Integer
intMaybe x = if isInt $ T.unpack x then Just $ read $ T.unpack x else Nothing

isFlt :: String -> Bool
isFlt num = isInt front && not (null $ drop 1 back) && and (map (`elem` ['0'..'9']) (drop 1 back))
  where (front, back) = splitWith (== '.') num

-- If this fails I'll be really mad.
fltMaybe :: T.Text -> Maybe Double
fltMaybe "" = Nothing
fltMaybe num =
  if isFlt $ T.unpack num
  then Just $ read $ T.unpack num
  else Nothing

blnMaybe :: T.Text -> Maybe Bool
blnMaybe x =
  case x of
    "True" -> Just True
    "False" -> Just False
    "true" -> Just True
    "false" -> Just False
    _ -> Nothing

chrMaybe :: String -> Maybe Char
chrMaybe ('\'':x:'\'':[]) = Just x
chrMaybe ('\'':'\\':x:'\'':[]) = Just $ toEsc x
chrMaybe _ = Nothing

-- Prototype version. Real version should query from a directory.
pthMaybe :: T.Text -> Maybe FilePath
pthMaybe "" = Nothing
pthMaybe str =
  if '/' `T.elem` str || str == "~"
  then Just $ T.unpack str
  else Nothing

pieces :: (Char -> Bool) -> T.Text -> [T.Text]
pieces f xs =
  if not $ T.null xs
  then front:pieces f (T.drop 1 back)
  else []
  where (front, back) = splitEsc f xs

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

tHeadMaybe :: T.Text -> Maybe Char
tHeadMaybe T.Empty = Nothing
tHeadMaybe (x T.:< _) = Just x

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe [a] = Just a
lastMaybe (_:xs) = lastMaybe xs

-- lines but \n and ,
-- liners :: String -> [String]
-- liners [] = []
-- liners xs = line:(liners $ drop 1 rest) where (line, rest) = splitWith (`elem` ['\n', ',']) xs

splitWith :: (a -> Bool) -> [a] -> ([a], [a])
splitWith _ [] = ([], [])
splitWith f list@(x:xs) =
  if f x
  then ([], list)
  else (x:front, back)
  where (front, back) = splitWith f xs

splitEsc :: (Char -> Bool) -> T.Text -> (T.Text, T.Text)
splitEsc _ T.Empty = (T.Empty, T.Empty)
splitEsc f ('\\' T.:< x T.:< xs) =
  (toEsc x T.:< front, back)
  where (front, back) = splitEsc f xs
splitEsc f list@(x T.:< xs) =
  if f x
  then (T.empty, list)
  else (T.cons x front, back)
  where (front, back) = splitEsc f xs

toEsc :: Char -> Char
toEsc x =
  case x of
    '0' -> '\0'
    'a' -> '\a'
    'b' -> '\b'
    'e' -> '\ESC'
    'f' -> '\f'
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    'v' -> '\v'
    _ -> x

matching :: Char -> Char
matching x =
  case x of
    '(' -> ')'
    '{' -> '}'
    '[' -> ']'
    '<' -> '>'
    ')' -> '('
    '}' -> '{'
    ']' -> '['
    '>' -> '<'
    _ -> x

splitInside :: Integer -> Char -> T.Text -> Maybe (T.Text, T.Text)
splitInside _ _ T.Empty = Nothing
splitInside acc opener (x T.:< txt) =
  if x `matches` opener
  then if acc == 1 then return (T.Empty, txt) else next (-1)
  else if x == opener then next 1 else next 0
  where
    next i =
      case splitInside (acc + i) opener txt of
        Nothing -> Nothing
        Just (front, back) -> Just (x T.:< front, back)

matches :: Char -> Char -> Bool
matches a b = (matching a) == b

takeDiff :: Eq a => [a] -> [a] -> [a]
takeDiff xs [] = xs
takeDiff [] _  = []
takeDiff (x:xs) (y:ys) = if x == y then takeDiff xs ys else x:xs

searchText :: T.Text -> T.Text -> [Integer]
searchText _ T.Empty = []
searchText needle haystack =
  if needle `T.isPrefixOf` haystack
  then 0 : map succ (searchText needle (T.drop 1 haystack))
  else map succ (searchText needle $ T.drop 1 haystack)
