module Token(tokenize, Tok, FmtPart, Prim) where
import Misc

data Tok =
  Idk String | Str String | Pth [String] | Int Int | Chr Char | Bln Bool | Pmt Prim |
  Bar | Dot | End | Tup [Tok] | Arr [Tok] | Fmt [FmtPart]
  deriving (Show)
data FmtPart =
  Text String | Expr Tok
  deriving (Show)
data Prim =
  Pint | Pchr | Pstr | Pflt | Pbln | Ptyp | Ppth
  deriving (Show)

tokenize :: String -> Either String [Tok]
tokenize x = fmap (\(a, b) -> if null b then map checkWord a else []) $ chunkify x

data Chunk =
  Word String | Character Char | String String | FString [Chunk] | Tuple [Chunk] | Array [Chunk] |
  Period | Pipe | Endline
  deriving (Show)

chunkify :: String -> Either String ([Chunk], String)
chunkify ('\'':'\\':x:'\'':xs) = do
  (rest, unchunked) <- chunkify xs
  Right (Character (toEsc x):rest, unchunked)
chunkify ('\'':x:'\'':xs) = do
  (rest, unchunked) <- chunkify xs
  Right (Character x:rest, unchunked)
chunkify ('f':'"':xs) = do
  (front, back) <- fmtChunk xs -- for fmtChunk, it can't rely on splitEsc. It will have to internally use its own version of splitEsc.
  (rest, unchunked) <- chunkify back
  Right (FString front:rest, unchunked)
chunkify list@(x:xs)
  |
  x `elem` " \t" =
    chunkify xs
  |
  x `elem` ",\r\n" = do
    (rest, unchunked) <- chunkify xs
    Right (Endline:rest, unchunked)
  |
  x == '.' = do
    (rest, unchunked) <- chunkify xs
    Right (Period:rest, unchunked)
  |
  x == '|' = do
    (rest, unchunked) <- chunkify xs
    Right (Pipe:rest, unchunked)
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
    else Left "Unclosed Tuple Delimiter \"({})\""
  |
  x == '[' = do
    (chunk, back) <- chunkify xs
    (rest, unchunked) <- chunkify $ drop 1 back
    if (headMaybe back) == (Just $ matching x)
    then Right (Array chunk:rest, unchunked)
    else Left "Unclosed Array Delimiter \"[]\""
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

checkWord :: Chunk -> Tok
checkWord x =
  case x of
    Period        -> Dot
    Pipe          -> Bar
    Endline       -> End
    String    str -> Str str
    Character chr -> Chr chr
    Tuple     tup -> Tup $ map checkWord tup
    Array     arr -> Arr $ map checkWord arr
    FString   fmt -> Fmt $ map (tok2fmt . checkWord) fmt
    Word      wrd -> checkType wrd
  where
    checkType x' =
      case pmtMaybe x' of
        Just pmt -> Pmt pmt
        Nothing  ->
          case blnMaybe x' of
            Just bln -> Bln bln
            Nothing  ->
              case intMaybe x' of
                Just int -> Int int
                Nothing  ->
                  case pthMaybe x' of
                    Just pth -> Pth pth
                    Nothing  -> Idk x'
    tok2fmt x' =
      case x' of
        Str s -> Text s
        _     -> Expr x'

pmtMaybe :: String -> Maybe Prim
pmtMaybe x =
  case x of
    "int" -> Just Pint
    "chr" -> Just Pchr
    "str" -> Just Pstr
    "flt" -> Just Pflt
    "bln" -> Just Pbln
    "typ" -> Just Ptyp
    "pth" -> Just Ppth
    _     -> Nothing
