import qualified System.Environment as Env

main = do
  env <- Env.getArgs
  putStrLn $ "Args: " ++ (show env)
  content <- readFile $ head env
  putStrLn $ "Content: " ++ content
  putStrLn $ (combine . lexer) content

combine :: [String] -> String
combine [] = []
combine (x:xs) = x ++ '\n':(combine xs)

-- The main loop acts like a for loop here. --
-- mainLoop = do

-- The lexer will have to be more error resilliant in the future. --
lexer :: String -> [String]
lexer [] = []
lexer [a] = [[a]]
lexer (' ':xs) = lexer xs
lexer ('\n':xs) = ",":(lexer xs)
lexer ('\t':xs) = lexer xs
lexer ('\r':xs) = lexer xs
lexer ('=':'=':xs) = "==":(lexer xs)
lexer ('/':'=':xs) = "/=":(lexer xs)
lexer ('<':'=':xs) = "<=":(lexer xs)
lexer ('>':'=':xs) = ">=":(lexer xs)
lexer ('&':'&':xs) = "&&":(lexer xs)
lexer ('|':'|':xs) = "||":(lexer xs)
lexer ('=':'>':xs) = "=>":(lexer xs)
lexer ('<':'-':xs) = "<-":(lexer xs)
lexer ('-':'>':xs) = "->":(lexer xs)
lexer ('.':'.':xs) = "..":(lexer xs)
lexer ('+':xs) = "+":(lexer xs)
lexer ('-':xs) = "-":(lexer xs)
lexer ('*':xs) = "*":(lexer xs)
lexer ('/':xs) = "/":(lexer xs)
lexer ('<':xs) = "<":(lexer xs)
lexer ('>':xs) = ">":(lexer xs)
lexer ('.':xs) = ".":(lexer xs)
lexer ('=':xs) = "=":(lexer xs)
lexer ('(':xs) = "(":(lexer xs)
lexer (')':xs) = ")":(lexer xs)
lexer ('{':xs) = "{":(lexer xs)
lexer ('}':xs) = "}":(lexer xs)
lexer ('[':xs) = "[":(lexer xs)
lexer (']':xs) = "]":(lexer xs)
lexer ('f':'"':xs) = ('f':(checkQuote ('"':xs))):(lexer rest)
  where
    checkQuote (x:y:ys) = if x /= '\\' && y == '"' then [x, y] else x:(checkQuote (y:ys))
    rest = drop (length $ checkQuote xs) xs
lexer ('"':xs) = (checkQuote ('"':xs)):(lexer rest)
  where
    checkQuote (x:y:ys) = if x /= '\\' && y == '"' then [x, y] else x:(checkQuote (y:ys))
    rest = drop (length $ checkQuote xs) xs
lexer ('\'':x:'\'':xs) = ['\'', x, '\'']:(lexer xs)
lexer ('|':xs) = "|":(lexer xs)
lexer (',':xs) = ",":(lexer xs)
lexer ('&':xs) = "&":(lexer xs)
lexer ('#':xs) = lexer (dropWhile (/= '\n') xs)
lexer xs = word:(lexer rest)
  where -- removed apostrophes because it is often used in sentences. '\'', 
    word = takeWhile (\a -> not $ a `elem` ['+', '-', '*', '/', '<', '>', '.', '=', '(', ')', '{', '}', '[', ']', '"', '|', '&', ';', ' ', '\t', '\r', '\n', '#']) xs
    rest = drop (length word) xs

data Token = Opr String | Var String | Lit String | Pip String | Cls String 
  deriving (Show)
