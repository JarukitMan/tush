module Misc where

isInt :: String -> Bool
isInt [] = True
isInt (x:xs) = if x `elem` ['0'..'9'] then isInt xs else False

intMaybe :: String -> Maybe Int
intMaybe x = if isInt x then Just $ read x else Nothing

blnMaybe :: String -> Maybe Bool
blnMaybe x =
  case x of
    "true" -> Just True
    "false" -> Just False
    _ -> Nothing

chrMaybe :: String -> Maybe Char
chrMaybe ('\'':x:'\'':[]) = Just x
chrMaybe ('\'':'\\':x:'\'':[]) = Just $ toEsc x
chrMaybe _ = Nothing

pthMaybe :: String -> Maybe [String]
pthMaybe xs =
  if x `elem` "./*?"
  then Just $ pieces (== '/') xs
  else Nothing
  where
    x =
      case headMaybe xs of
        Just x' -> x'
        Nothing -> '\0'

pieces :: (Char -> Bool) -> String -> [String]
pieces _ [] = []
pieces f xs = front:pieces f (drop 1 back)
  where (front, back) = splitEsc f xs

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

-- lines but \n and ,
liners :: String -> [String]
liners [] = []
liners xs = line:(liners $ drop 1 rest) where (line, rest) = splitWith (`elem` ['\n', ',']) xs

splitWith :: (a -> Bool) -> [a] -> ([a], [a])
splitWith _ [] = ([], [])
splitWith f list@(x:xs) =
  if f x
  then ([], xs)
  else (x:front, back)
  where (front, back) = splitWith f list

splitEsc :: (Char -> Bool) -> String -> (String, String)
splitEsc _ [] = ([], [])
splitEsc f ('\\':x:xs) =
  (toEsc x:front, back)
  where (front, back) = splitEsc f xs
splitEsc f list@(x:xs) =
  if f x
  then ([], list)
  else (x:front, back)
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
    _ -> x

matches :: Char -> Char -> Bool
matches a b = (matching a) == b
