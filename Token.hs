module Token where
import Misc

data Tok =
  Idk String | Str String | Int Int | Chr Char | Bln Bool | Pmt Prim |
  Bar | Dot | Tup [Tok] | Arr [Tok] | Fmt [FmtPart]
  deriving (Show)
data FmtPart =
  Text String | Expr Tok
  deriving (Show)
data Prim =
  Pint | Pchr | Pstr | PFlt | Pbln | PTyp
  deriving (Show)

tokenize :: String -> Either String [Tok]
tokenize x = undefined

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
