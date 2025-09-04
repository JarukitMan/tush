{-# LANGUAGE OverloadedStrings #-}
module Tssl.Token(tokenize) where
import Misc
import Tssl.Data
import qualified Data.Text as T

tokenize :: Memory -> T.Text -> Either T.Text [Token]
tokenize memory x = case (chunkify) x of
  Left errmsg -> Left errmsg
  Right (chunks, rest) ->
    if T.null rest
    then Right $ map (checkWord memory) (preprocess chunks)
    else Left "Unmatched Right Delimiter ({[]})"

data Chunk =
  Word T.Text | Character Char | String T.Text | FString [Chunk] | Tuple [Chunk] | Array [Chunk]
  deriving (Show, Eq)

-- Catch chars
chunkify :: T.Text -> Either T.Text ([Chunk], T.Text)
chunkify ('\'' T.:< '\\' T.:< x T.:< '\''T.:< xs) = do
  (rest, unchunked) <- chunkify (xs)
  Right (Character (toEsc x):rest, unchunked)
chunkify ('\'' T.:< x T.:< '\'' T.:< xs) = do
  (rest, unchunked) <- chunkify xs
  Right (Character x:rest, unchunked)

-- Catch formatted strings
chunkify ('f' T.:< '"'T.:< xs) = do
  (front, back) <- fmtChunk xs -- for fmtChunk, it can't rely on splitEsc. It will have to internally use its own version of splitEsc.
  (rest, unchunked) <- chunkify back
  Right (FString front:rest, unchunked)

-- Catch dirs that lead with dots.
chunkify ('.'T.:< '.' T.:< xs) = do
    (rest, unchunked) <- chunkify xs
    Right (Word "..":rest, unchunked)

chunkify T.Empty = Right ([], T.empty)

chunkify list@(x T.:< xs)
  |
  x `T.elem` " \t" =
    chunkify xs
  |
  x `T.elem` ";,\r\n" = do
    (rest, unchunked) <- chunkify xs
    Right (Word ",":rest, unchunked)
  |
  x `T.elem` ":|" = do
    (rest, unchunked) <- chunkify xs
    Right (Word (T.singleton x):rest, unchunked)
  |
  x `T.elem` "})]" =
    Right ([], list)
  |
  x == '#' =
    chunkify $ T.dropWhile (/= '\n') xs
  |
  x == '"' =
    let
      (front, back) = splitEsc (== '"') xs
    in
      if T.null back
      then Left "Unclosed String"
      else do
          (rest, unchunked) <- chunkify $ T.drop 1 back
          Right (String front:rest, unchunked)
  |
  x `T.elem` "({" = do
    (chunk, back) <- chunkify xs
    (rest, unchunked) <- chunkify $ T.drop 1 back
    if (tHeadMaybe back) == (Just $ matching x)
    then Right (Tuple chunk:rest, unchunked)
    else Left "Unclosed Left Tuple Delimiter \"({})\""
  |
  x == '[' = do
    (chunk, back) <- chunkify xs
    (rest, unchunked) <- chunkify $ T.drop 1 back
    if (tHeadMaybe back) == (Just $ matching x)
    then Right (Array chunk:rest, unchunked)
    else Left "Unclosed Left Array Delimiter \"[]\""
  |
  otherwise = do
    let (front, back) = splitEscEx (`T.elem` " \t\r\n,{}()[]:;|") list
    case back of
      _ -> do
        (rest, unchunked) <- chunkify back
        Right (Word front:rest, unchunked)
  where
    splitEscEx :: (Char -> Bool) -> T.Text -> (T.Text, T.Text)
    -- Just copied from misc with an extra condition of ".."
    splitEscEx _ ('.' T.:< '.' T.:< ts) = (T.Empty, ".." `T.append` ts)
    splitEscEx _ T.Empty = (T.Empty, T.Empty)
    splitEscEx f ('\\' T.:< t T.:< ts) =
      (toEsc t T.:< front, back)
      where (front, back) = splitEscEx f ts
    splitEscEx f l@(t T.:< ts) =
      if f t
      then (T.empty, l)
      else (T.cons t front, back)
      where (front, back) = splitEscEx f ts


fmtChunk :: T.Text -> Either T.Text ([Chunk], T.Text)
fmtChunk list
  |
  T.null list = Left "Unclosed Formatted String"
  |
  x == '"' = Right ([], xs)
  |
  x == '{' = do
    (chunked, unchunked) <- chunkify xs
    if tHeadMaybe unchunked == Just '}'
    then do
      (back, rest) <- fmtChunk $ T.drop 1 unchunked
      Right (Tuple chunked:back, rest)
    else Left "Unclosed Formatted String Tuple Delimiter \"{}\""
  |
  otherwise = do
  let (front, xs') = splitEsc (`T.elem` "\"{") list
  (back, rest) <- fmtChunk xs'
  Right (String front:back, rest)
  where
    x = T.head list
    xs = T.drop 1 list

checkWord :: Memory -> Chunk -> Token
checkWord memory x =
  case x of
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
        Just (Val  _) -> Var   x'
        Nothing       ->
          case pmtMaybe x' of
            Just typ -> Typ typ
            Nothing  ->
              case blnMaybe x' of
                Just bln -> Bln bln
                Nothing  ->
                  case intMaybe x' of
                    Just int -> Int int
                    Nothing  ->
                      case fltMaybe x' of
                        Just flt -> Flt flt
                        Nothing  ->
                          case pthMaybe x' of
                            Just pth -> Pth pth
                            Nothing  -> Wrd x'
    tok2fmt x' =
      case x' of
        Str s -> Left  s
        _     -> Right x'

pmtMaybe :: T.Text -> Maybe Type
pmtMaybe x =
  case x of
    "Int" -> Just Tint
    "Chr" -> Just Tchr
    "Str" -> Just Tstr
    "Flt" -> Just Tflt
    "Bln" -> Just Tbln
    "Typ" -> Just Ttyp
    "Pth" -> Just Tpth
    "Any" -> Just Tany
    _     -> Nothing

preprocess :: [Chunk] -> [Chunk]
preprocess [] = []
preprocess (Tuple tup:xs) = Tuple (preprocess tup):preprocess xs
preprocess (FString tup:xs) = FString (preprocess tup):preprocess xs
preprocess (Array tup:xs) = Array (preprocess tup):preprocess xs
preprocess (Word "for":x@(Word _):Word "in":xs) =
      case splitWith (\t -> case t of {Tuple _ -> True ; _ -> False}) xs of
        ([], rest) ->
          case rest of
            (Tuple y:back) -> Word "foreach":Tuple [x, Word ",", Tuple (preprocess y)]:preprocess back
            _ -> Word "for":x:Word "in":preprocess xs
        (front, back) -> Word "foreach":Tuple [x, Tuple (preprocess front)]:preprocess back
preprocess (Word "for":Tuple (x@(Word _):Word "in":ys):xs) = Word "foreach" :Tuple(x:Word ",":ys):xs
preprocess (chunk:ts)
  |
  chunk `elem` [Word "if", Word "for", Word "while"] =
    case splitWith (\t -> case t of {Tuple _ -> True ; _ -> False}) ts of
      ([], _) -> chunk:preprocess ts
      (_, []) -> chunk:preprocess ts
      (front, back) -> chunk:Tuple (preprocess front):preprocess back
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
