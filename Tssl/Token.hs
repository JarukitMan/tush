module Tssl.Token(tokenize) where
import Misc
-- import qualified Data.Map as M 
import Tssl.Data
-- import Data.Word

tokenize :: Memory -> String -> Either String [Token]
tokenize memory x = case (chunkify) x of
  Left errmsg -> Left errmsg
  Right (chunks, rest) ->
    if null rest
    then Right $ map (checkWord memory) (preprocess chunks)
    else Left "Unmatched Right Delimiter ({[]})"

data Chunk =
  Word String | Character Char | String String | FString [Chunk] | Tuple [Chunk] | Array [Chunk]
  -- Period | Pipe | Endline
  deriving (Show, Eq)

chunkify :: String -> Either String ([Chunk], String)
-- Catch chars
chunkify ('\'':'\\':x:'\'':xs) = do
  (rest, unchunked) <- chunkify xs
  Right (Character (toEsc x):rest, unchunked)
chunkify ('\'':x:'\'':xs) = do
  (rest, unchunked) <- chunkify xs
  Right (Character x:rest, unchunked)

-- Catch formatted strings
chunkify ('f':'"':xs) = do
  (front, back) <- fmtChunk xs -- for fmtChunk, it can't rely on splitEsc. It will have to internally use its own version of splitEsc.
  (rest, unchunked) <- chunkify back
  Right (FString front:rest, unchunked)

-- Catch dirs that lead with dots.
chunkify ('.':'.':xs) = do
    (rest, unchunked) <- chunkify xs
    Right (Word "..":rest, unchunked)
chunkify ('.':'/':xs) = do
    let (front, xs') = splitEsc (== ' ') xs
    (rest, unchunked) <- chunkify xs'
    Right (Word ('.':'/':front):rest, unchunked)

chunkify list@(x:xs)
  |
  x `elem` " \t" =
    chunkify xs
  |
  x `elem` ",\r\n" = do
    (rest, unchunked) <- chunkify xs
    Right (Word ",":rest, unchunked)
  |
  x == '.' = do
    (rest, unchunked) <- chunkify xs
    Right (Word ".":rest, unchunked)
  |
  x == '|' = do
    (rest, unchunked) <- chunkify xs
    Right (Word "|":rest, unchunked)
  |
  x `elem` "})]" =
    Right ([], list)
  |
  x == '#' =
    chunkify $ dropWhile (/= '\n') xs
  |
  x == '"' =
    let (front, back) = splitEsc (== '"') xs
    in case back of
      [] -> Left "Unclosed String"
      _  -> do
        (rest, unchunked) <- chunkify $ drop 1 back
        Right (String front:rest, unchunked)
  |
  x `elem` "({" = do
    (chunk, back) <- chunkify xs
    (rest, unchunked) <- chunkify $ drop 1 back
    if (headMaybe back) == (Just $ matching x)
    then Right (Tuple chunk:rest, unchunked)
    else Left "Unclosed Left Tuple Delimiter \"({})\""
  |
  x == '[' = do
    (chunk, back) <- chunkify xs
    (rest, unchunked) <- chunkify $ drop 1 back
    if (headMaybe back) == (Just $ matching x)
    then Right (Array chunk:rest, unchunked)
    else Left "Unclosed Left Array Delimiter \"[]\""
  |
  otherwise = do
    let (front, back) = splitEsc (`elem` " \t\r\n,{}()[].\"|") list
    (rest, unchunked) <- chunkify back
    Right $ (Word front:rest, unchunked)
chunkify [] = Right ([], [])

fmtChunk :: String -> Either String ([Chunk], String)
fmtChunk [] = Left "Unclosed Formatted String"
fmtChunk list@(x:xs)
  |
  x == '"' = Right ([], xs)
  |
  x == '{' = do
    (chunked, unchunked) <- chunkify xs
    if headMaybe unchunked == Just '}'
    then do
      (back, rest) <- fmtChunk $ drop 1 unchunked
      Right (Tuple chunked:back, rest)
    else Left "Unclosed Formatted String Tuple Delimiter \"{}\""
  |
  otherwise = do
  let (front, xs') = splitEsc (`elem` "\"{") list
  (back, rest) <- fmtChunk xs'
  Right (String front:back, rest)

checkWord :: Memory -> Chunk -> Token
checkWord memory x =
  case x of
    -- Period        -> Dot
    -- Pipe          -> Bar
    -- Endline       -> End
    String    str -> Str str
    Character chr -> Chr chr
    Tuple     tup -> Tup $ map (checkWord memory) tup
    Array     arr -> Arr $ map (checkWord memory) arr
    FString   fmt -> Fmt $ map (tok2fmt . checkWord memory) fmt
    Word      wrd -> checkType wrd
  where
    checkType x' =
      case getMem memory x' of
        Just (Op r _) -> Opr r x'
        Just (Val _)        -> Var x'
        Nothing               ->
          case pmtMaybe x' of
            Just typ -> Typ typ
            Nothing  ->
              case blnMaybe x' of
                Just bln -> Bln bln
                Nothing  ->
                  case intMaybe x' of
                    Just int -> Int int
                    Nothing  ->
                      case pthMaybe x' of
                        Just pth -> Pth pth
                        Nothing  -> Str x'
    tok2fmt x' =
      case x' of
        Str s -> Left  s
        _     -> Right x'

pmtMaybe :: String -> Maybe Type
pmtMaybe x =
  case x of
    "Int" -> Just Tint
    "Chr" -> Just Tchr
    "Str" -> Just Tstr
    "Flt" -> Just Tflt
    "Bln" -> Just Tbln
    "Typ" -> Just Ttyp
    "Pth" -> Just Tpth
    _     -> Nothing

-- preprocess :: String -> String
-- preprocess ('c':'o':'n':'t':'i':'n':'u':'e':' ':xs) = "return () " ++ xs
-- preprocess ('o':'p':'r':' ':xs) = "opr" ++ ('(':front) ++ (')':back)
--   where (front, back) = splitWith (== '=') xs
-- preprocess ('i':'f':' ':xs) =
--   if not $ null $ filter (\x -> not $ x `elem` " \t\r\n") front
--   then "if" ++ ('(':front) ++ (')':back)
--   else "if " ++ preprocess xs
--   where (front, back) = splitWith (`elem` "({") xs
-- preprocess ('f':'o':'r':' ':xs) =
--   case words front of
--     (x:"in":xs') -> "foreach" ++ ('(':x) ++ (',':' ':unwords xs') ++ (')':back)
--     _ ->
--       if not $ null $ filter (\x -> not $ x `elem` " \t\r\n") front
--       then "for" ++ ('(':front) ++ (')':back)
--       else "for " ++ preprocess xs
--   where (front, back) = splitWith (`elem` "({") xs
-- preprocess ('w':'h':'i':'l':'e':' ':xs) =
--   if not $ null $ filter (\x -> not $ x `elem` " \t\r\n") front
--   then "while" ++ ('(':front) ++ (')':back)
--   else "while " ++ preprocess xs
--   where (front, back) = splitWith (`elem` "({") xs
-- preprocess [] = []
-- preprocess (x:xs) = x:(preprocess xs)
preprocess :: [Chunk] -> [Chunk]
preprocess [] = []
preprocess (chunk:ts)
  |
  chunk `elem` [Word "if", Word "for", Word "while"] =
    case splitWith (\t -> case t of {Tuple _ -> True ; _ -> False}) ts of
      ([], _) -> chunk:preprocess ts
      (_, []) -> chunk:preprocess ts
      (front, back) -> chunk:Tuple front:preprocess back
  |
  chunk == Word "continue" = Word "return":Tuple []:preprocess ts
  |
  chunk == Word "opr" =
    case splitWith (/= Word "=") ts of
      ([], _) -> chunk:preprocess ts
      (_, []) -> chunk:preprocess ts
      (front, back) -> chunk:Tuple front:preprocess back
  |
  otherwise = chunk:preprocess ts
