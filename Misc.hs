module Misc where

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
isInt [] = True
isInt (x:xs) = if x `elem` ['0'..'9'] then isInt xs else False

intMaybe :: String -> Maybe Integer
intMaybe x = if isInt x then Just $ read x else Nothing

blnMaybe :: String -> Maybe Bool
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
pthMaybe :: String -> Maybe FilePath
pthMaybe [] = Nothing
pthMaybe str =
  if '/' `elem` str || str == "~"
  then Just str
  else Nothing

pieces :: (Char -> Bool) -> String -> [String]
pieces _ [] = []
pieces f xs = front:pieces f (drop 1 back)
  where (front, back) = splitEsc f xs

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe [a] = Just a
lastMaybe (_:xs) = lastMaybe xs

-- lines but \n and ,
liners :: String -> [String]
liners [] = []
liners xs = line:(liners $ drop 1 rest) where (line, rest) = splitWith (`elem` ['\n', ',']) xs

splitWith :: (a -> Bool) -> [a] -> ([a], [a])
splitWith _ [] = ([], [])
splitWith f list@(x:xs) =
  if f x
  then ([], list)
  else (x:front, back)
  where (front, back) = splitWith f xs

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

takeDiff :: Eq a => [a] -> [a] -> [a]
takeDiff xs [] = xs
takeDiff [] _  = []
takeDiff (x:xs) (y:ys) = if x == y then takeDiff xs ys else x:xs
