module Misc where

isInt :: String -> Bool
isInt [] = True
isInt (x:xs) = if x `elem` ['0'..'9'] then isInt xs else False

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

-- lines but \n and ,
liners :: String -> [String]
liners [] = []
liners xs = line:(liners rest) where (line, rest) = splitWhile (\x -> not $ x `elem` ['\n', ',']) xs

splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile _ [] = ([], [])
splitWhile f list@(x:xs) =
  if f x
  then (x:front, back)
  else ([], xs)
  where (front, back) = splitWhile f list
