import qualified System.Environment as Env

main = do
  env <- Env.getArgs
  putStrLn $ "Args: " ++ (show env)
  content <- readFile $ head env
  putStrLn $ "Content: " ++ content
  putStrLn $ (combine . (map show) . fst . (lexMultiple '_')) content

combine :: [String] -> String
combine [] = []
combine (x:xs) = x ++ '\n':(combine xs)

-- The main loop acts like a for loop here.
-- mainLoop = do

data Lexeme = End | Ret | And | Or | Equ | Ueq |
              Wrt | Apn | Hre | Pip | Unb | Add |
              Sub | Mpy | Div | Lse | Mre | Les |
              Mor | Rng | Acs | Cma | Col | Cop |
              Ccl | Pop | Pcl | Bop | Bcl | Apo |
              Fop | Quo | Unknown String | Fmt [Lexeme] |
              Tup [Lexeme] | Arr [Lexeme] | Asn |
              Str String | Chr Char | None
  deriving (Show, Eq)

-- TODO: Handle Errors (With an Either type.)
--       Fix the parsing as commented in array example.
-- Lexes all lines until it encounters the character passed to it.
-- Decided not to put none here.
lexMultiple :: Char -> String -> ([Lexeme], String)
lexMultiple _ [] = ([], [])
lexMultiple x exprs@(y:ys)
  |
  x == y = ([], ys)
  |
  null line = (otherlines, rest)
  |
  otherwise = case x of
    ']' -> (Arr line:otherlines, rest)
    otherwise -> case length line of
      0 -> (otherlines, rest)
      1 -> (head line:otherlines, rest)
      otherwise -> (Tup line:otherlines, rest)
  where
    (line, xs) = lexOne exprs
    (otherlines, rest) = lexMultiple x xs

-- Just does it until the line ends and returns the parsed part and the rest of the string.
lexOne :: String -> ([Lexeme], String)
lexOne ('#':xs) = lexOne (dropWhile (/= '\n') xs)
lexOne list
  |
  elem front " \t\r" = (lexOne $ drop 1 list)
  |
  elem front "\n," = ([], drop 1 list)
  |
  elem front "({[" = case length closure of
    0 -> (None:line, rest)
    1 -> (head closure:line, rest)
    otherwise -> (Tup closure:line, rest)
  |
  elem front ")}]" = ([], list)
    where
      front = case headMaybe list of
                Just x -> x
                Nothing -> '\n'
      (closure, xs) = lexMultiple (matching front) (drop 1 list)
      (line, rest) = lexOne xs
lexOne ('\'':x:'\'':xs) = (Chr x:line, rest)
  where
    (line, rest) = lexOne xs
lexOne ('\'':'\\':x:'\'':xs) = ((Chr $ toEsc x):line, rest)
  where
    (line, rest) = lexOne xs
lexOne ('"':xs) = (Str line:ys, rest)
  where
    (line, expr) = strParse xs
    (ys, rest) = lexOne expr
lexOne ('f':'"':xs) = (Fmt line:ys, rest)
  where
    (line, expr) = fmtParse xs
    (ys, rest) = lexOne expr
lexOne [] = ([], [])
lexOne xs = (lexeme:rest, restString)
  where
    lexemes = [("\n", End),
              ("&&", And), ("||", Or), ("==", Equ), ("/=", Ueq), ("->", Wrt), ("=>", Apn), ("<-", Hre),
              (",", End), ("|", Pip), ("&", Unb), ("+", Add), ("-", Sub), ("*", Mpy),
              ("/", Div), ("<=", Lse), (">=", Mre), ("<", Les), (">", Mor),
              ("..", Rng), (".", Acs), (",", Cma), (":", Col), ("=", Asn),
              ("{", Cop), ("}", Ccl), ("(", Pop), (")", Pcl), ("[", Bop), ("]", Bcl)]
    (lexeme, string) = checkPrefix lexemes xs
    (rest, restString) = lexOne string

-- Helpers

fmtParse :: String -> ([Lexeme], String)
fmtParse [] = ([], [])
fmtParse ('"':xs) = ([], xs)
fmtParse ('{':xs) = case length word of
  0 -> (None:line, rest)
  1 -> (head word:line, rest)
  otherwise -> (Tup word:line, rest)
  where
    (word, ys) = if '}' `elem` xs then lexMultiple '}' xs else ([Str ('{':z)], '"':zs)
      where
        (z, zs) = strParse xs
    (line, rest) = fmtParse ys
fmtParse xs = (Str word:line, rest)
  where
    (word, ys) =
      let
        takeStr [] = ([], [])
        takeStr ('\\':x:xs) = (toEsc x:z, zs)
          where
            (z, zs) = takeStr xs
        takeStr (x:xs)
          |
          x `elem` "\"{" = ([], x:xs)
          |
          otherwise = (x:z, zs)
          where
            (z, zs) = takeStr xs
      in
        takeStr xs
    (line, rest) = fmtParse ys -- TODO: Horribly unoptimized.

strParse :: String -> (String, String)
strParse [] = ([], [])
strParse ('"':xs) = ([], xs)
strParse ('\\':x:xs) = ((toEsc x):word, rest)
  where
    (word, rest) = strParse xs
strParse (x:xs) = (x:word, rest)
  where
    (word, rest) = strParse xs

compareLists :: [a] -> [b] -> Ordering
compareLists [] [] = EQ
compareLists [] _ = LT
compareLists _ [] = GT
compareLists (x:xs) (y:ys) = compareLists xs ys
checkPrefix :: [(String, Lexeme)] -> String -> (Lexeme, String)
checkPrefix [] line = getWord
  where
    shortlexs = ['\n', '\r', '\t', ' ',
                 '&', '|', ',', '=', '<', '>',
                 '+', '-', '*', '/', '.', ',', ':',
                 '{', '}', '(', ')', '[', ']', '"']
    word = takeWhile (\x -> not $ x `elem` shortlexs) line
    getWord = (Unknown word, (drop $ length word) line)
checkPrefix ((x, y):xs) line
  |
  x `compareLists` line == GT = checkPrefix xs line
  |
  checked == x = (y, rest)
  |
  otherwise = checkPrefix xs line
    where
      checked = take (length x) line
      rest = drop (length x) line

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
    x -> x

isInt :: String -> Bool
isInt [] = True
isInt (x:xs) = if x `elem` ['0'..'9'] then isInt xs else False

matching :: Char -> Char
matching '(' = ')'
matching '{' = '}'
matching '[' = ']'

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x
