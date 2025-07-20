{-# LANGUAGE OverloadedStrings #-}
module Tssl.Data where

import qualified Data.Map.Strict as M
import Data.Char(ord)
import Data.Word(Word8)
import Data.List(intercalate)
-- import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- import qualified Data.Text.Encoding as T
-- import qualified Data.Text.Encoding.Error as T
import Misc
import System.Process
import System.IO
-- import System.Exit
-- import System.Environment
import Control.Exception

-- Accepts the name and maps it to the value.
-- Variables and operators use the same namespace.
-- Maps name -> value
-- list for scope, kept track of in main and altered in eval 
type Memory = [M.Map T.Text Data]

newscope :: Memory -> Memory
newscope mem = M.empty:mem

insertMem :: T.Text -> Data -> Memory -> Memory
insertMem dname dat (m:ms) =  (M.insert dname dat m):ms
insertMem dname dat [] =  [M.insert dname dat M.empty]

insertVar :: T.Text -> Value -> Memory -> Memory
insertVar vname val (m:ms) = (M.insert vname (Val $ Right val) m):ms
insertVar vname val [] = [M.insert vname (Val $ Right val) M.empty]

updateVar :: T.Text -> Value -> Memory -> Memory
updateVar vname val (m:ms) =
  case vname `M.lookup` m of
    Nothing -> m:(updateVar vname val ms)
    Just _  -> (M.insert vname (Val $ Right val) m):ms
updateVar _ _ [] = []

insertBlankVar :: T.Text -> Type -> Memory -> Memory
insertBlankVar vname typ (m:ms) = (M.insert vname (Val $ Left typ) m):ms
insertBlankVar vname typ [] =     [M.insert vname (Val $ Left typ) M.empty]
-- Op in the Value enumuration returns the cardinality, scope and
-- a map from input types to the operand names, body, and output type.
-- This is done so that one single namespace is used
-- for both variables and operators while allowing overloading. 
-- Op is rank, map from types to body
-- The opmap is converted to (Map left -> (Map right -> op, op if right doesn't match (any)), (Map right -> op, op if both don't match (any)) if left doesn't match (any))
data Data = Op Word8 OpMap | Val (Either Type Value)
  deriving (Show, Eq)
type OpMap = (M.Map Type (M.Map Type Operator, Maybe Operator), Maybe (M.Map Type Operator, Maybe Operator))

data Token =
  Wrd T.Text | Var T.Text | Opr Word8 T.Text | Str T.Text | Pth FilePath | Int Integer | Flt Double | Chr Char | Bln Bool | Typ Type |
  Tup [Token] | Arr [Token] | Fmt [Either T.Text Token]
  deriving (Eq, Ord)

-- This is for tokens so it's fine
instance Show Token where
  show :: Token -> String
  show t =
    case t of
      Wrd   wrd -> "\ESC[30m"    ++ T.unpack wrd                   ++ "\ESC[0m"
      Var   var -> "\ESC[30m"    ++ T.unpack var                   ++ "\ESC[0m"
      Opr _ opr -> "\ESC[31m"    ++ T.unpack opr                   ++ "\ESC[0m"
      Str   str -> "\ESC[32m\""  ++ T.unpack str                   ++ "\"\ESC[0m"
      Pth   pth -> "\ESC[33m"    ++ pth                            ++ "\ESC[0m"
      Int   int -> "\ESC[34m"    ++ show int                       ++ "\ESC[0m"
      Flt   flt -> "\ESC[34m"    ++ show flt                       ++ "\ESC[0m"
      Chr   chr -> "\ESC[35m'"   ++ chr                            :  "'\ESC[0m"
      Bln   bln -> "\ESC[36m"    ++ show bln                       ++ "\ESC[0m"
      Typ   typ -> "\ESC[37m"    ++ show typ                       ++ "\ESC[0m"
      Tup   tup -> "\ESC[38m("   ++ intercalate " " (map show tup) ++ ")\ESC[0m"
      Arr   arr -> "\ESC[39m["   ++ intercalate " " (map show arr) ++ "]\ESC[0m"
      Fmt   fmt ->
        "\ESC[32mf\""
        ++ intercalate " "
        (map (\x -> case x of {
        Left str -> "\ESC[32m" ++ T.unpack str ++ "\ESC[0m" ;
        Right tup -> show tup}) fmt) ++
        "\ESC[32m\"\ESC[0m"

checkWrd :: Memory -> Token -> Token
checkWrd mem tok =
  case tok of
    Wrd word ->
      case mem `getMem` word of
        Nothing -> tok
        Just (Op r _)  -> Opr r word
        Just (Val  _)  -> Var   word
    Tup toks -> Tup $ map (checkWrd mem) toks
    Arr toks -> Arr $ map (checkWrd mem) toks
    Fmt prts -> Fmt $ map (\x -> case x of {Left txts -> Left txts; Right tup -> Right $ checkWrd mem tup}) prts
    _ -> tok

-- data Word =
--   Var String | Opr Word8 String | Str String | Pth String | Int Integer | Chr Char | Bln Bool | Typ Type |
--   Tup Expression | Arr Expression | Fmt [Either String Expression]
--   deriving (Eq, Ord, Show)

-- Strict form of token.
data Value =
  Int' Integer | Flt' Double | Chr' Char | Str' T.Text | Bln' Bool | Pth' FilePath | Typ' Type |
  Arr' Type [Value] | Tup' [Value] | Break | Out' Handle ProcessHandle
  -- deriving (Eq)

emptish :: Value -> Bool
emptish v =
  case v of
    Tup' [] -> True
    Tup' vs -> and (map emptish vs)
    _       -> False

instance Eq Value where
  (==) :: Value -> Value -> Bool
  v1 == v2 =
    case (v1, v2) of
      -- I don't want to repeat the Ord again, lol.
      (Int' a, Int' b) -> a == b
      (Int' a, Flt' b) -> (fromInteger a) == b
      (Flt' a, Int' b) -> a == (fromInteger b)
      (Flt' a, Flt' b) -> a == b
      (Chr' a, Int' b) -> ord a == (fromInteger b)
      (Int' a, Chr' b) -> (fromInteger a) == ord b
      (Chr' a, Chr' b) -> a == b
      (Str' a, Str' b) -> a == b
      (Bln' a, Bln' b) -> a == b
      (Pth' a, Pth' b) -> a == b
      (Typ' a, Typ' b) -> a == b
      (Arr' _ a, Arr' _ b) -> a == b
      (Tup' a, Tup' b) -> a == b
      _                -> False
instance Ord Value where
  compare :: Value -> Value -> Ordering
  compare v1 v2 =
    case (v1, v2) of
      (Int' a, Int' b) -> a `compare` b
      (Int' a, Flt' b) -> (fromInteger a) `compare` b
      (Int' a, Chr' b) -> (fromInteger a) `compare` (ord b)
      (Int' a, Str' b) -> a `compare` (fromIntegral $ T.length b)
      (Int' a, Bln' b) -> if b then a `compare` 1 else a `compare` 0
      (Int' a, Pth' b) -> a `compare` (fromIntegral $ length (pieces (== '/') (T.pack b)))
      (Int' a, Typ' b) -> show a `compare` show b
      (Int' a, Arr' _ b) ->
        case b of
          [c] -> Int' a `compare` c
          _   -> LT
      (Int' a, Tup' b) ->
        case b of
          [c] -> Int' a `compare` c
          _   -> LT
      (Flt' a, Int' b) -> a `compare` (fromIntegral b)
      (Flt' a, Flt' b) -> a `compare` b
      (Flt' a, Chr' b) -> a `compare` (fromIntegral $ ord b)
      (Flt' a, Str' b) -> a `compare` (fromIntegral $ T.length b)
      (Flt' a, Bln' b) -> if b then a `compare` 1 else a `compare` 0
      (Flt' a, Pth' b) -> a `compare` (fromIntegral $ length (pieces (== '/') (T.pack b)))
      (Flt' a, Typ' b) -> show a `compare` show b
      (Flt' a, Arr' _ b) ->
        case b of
          [c] -> Flt' a `compare` c
          _   -> LT
      (Flt' a, Tup' b) ->
        case b of
          [c] -> Flt' a `compare` c
          _   -> LT
      (Chr' a, Int' b) -> (ord a) `compare` (fromIntegral b)
      (Chr' a, Flt' b) -> (fromIntegral $ ord a) `compare` b
      (Chr' a, Chr' b) -> a `compare` b
      (Chr' a, Str' b) ->
        case T.uncons b of
          Just (c, "") -> a `compare` c
          _   -> LT
      (Chr' a, Bln' b) -> if b then a `compare` 'T' else a `compare` 'F'
      (Chr' a, Pth' b) -> (ord a) `compare` (fromIntegral $ length (pieces (== '/') (T.pack b)))
      (Chr' _, Typ' _) -> LT
      (Chr' a, Arr' _ b) ->
        case b of
          [c] -> Chr' a `compare` c
          _   -> LT
      (Chr' a, Tup' b) ->
        case b of
          [c] -> Chr' a `compare` c
          _   -> LT
      (Str' a, Int' b) -> b `compare` (fromIntegral $ T.length a)
      (Str' a, Flt' b) -> b `compare` (fromIntegral $ T.length a)
      (Str' a, Chr' b) ->
        case T.uncons a of
          Just (c, "") -> b `compare` c
          _   -> GT
      (Str' a, Str' b) -> a `compare` b
      (Str' a, Bln' b) -> a `compare` T.show b
      (Str' a, Pth' b) -> a `compare` (T.pack b)
      (Str' a, Typ' b) -> a `compare` T.show b
      (Str' a, Arr' _ b) ->
        case b of
          [c] -> Str' a `compare` c
          _   -> LT
      (Str' a, Tup' b) ->
        case b of
          [c] -> Str' a `compare` c
          _   -> LT
      (Bln' a, Int' b) -> if a then b `compare` 1 else b `compare` 0
      (Bln' a, Flt' b) -> if a then b `compare` 1 else b `compare` 0
      (Bln' a, Chr' b) -> if a then b `compare` 'T' else b `compare` 'F'
      (Bln' a, Str' b) -> T.show a `compare` b
      (Bln' a, Bln' b) -> a `compare` b
      (Bln' a, Pth' b) -> T.show a `compare` (T.pack b)
      (Bln' _, Typ' _) -> LT
      (Bln' a, Arr' _ b) ->
        case b of
          [c] -> Bln' a `compare` c
          _   -> LT
      (Bln' a, Tup' b) ->
        case b of
          [c] -> Bln' a `compare` c
          _   -> LT
      (Pth' a, Int' b) -> (fromIntegral $ length (pieces (== '/') (T.pack a))) `compare` b
      (Pth' a, Flt' b) -> b `compare` (fromIntegral $ length (pieces (== '/') (T.pack a)))
      (Pth' a, Chr' b) -> (ord b) `compare` (fromIntegral $ length (pieces (== '/') (T.pack a)))
      (Pth' a, Str' b) -> (T.pack a) `compare` b
      (Pth' a, Bln' b) -> a `compare` show b
      (Pth' a, Pth' b) -> a `compare` b
      (Pth' a, Typ' b) -> a `compare` show b
      (Pth' a, Arr' _ b) ->
        case b of
          [c] -> Pth' a `compare` c
          _   -> LT
      (Pth' a, Tup' b) ->
        case b of
          [c] -> Pth' a `compare` c
          _   -> LT
      (Typ' a, Int' b) -> T.show a `compare` T.show b
      (Typ' a, Flt' b) -> T.show a `compare` T.show b
      (Typ' _, Chr' _) -> GT
      (Typ' a, Str' b) -> T.show a `compare` b
      (Typ' _, Bln' _) -> GT
      (Typ' a, Pth' b) -> show a `compare` b
      (Typ' a, Typ' b) -> a `compare` b
      (Typ' a, Arr' _ b) ->
        case b of
          [c] -> Typ' a `compare` c
          _   -> LT
      (Typ' a, Tup' b) ->
        case b of
          [c] -> Typ' a `compare` c
          _   -> LT
      (Arr' _ a, b) ->
        case a of
          [c] -> c `compare` b
          _ ->
            case b of
              Arr' _ c -> length a `compare` length c
              Tup' c -> length a `compare` length c
              _ -> GT
      (Tup' a, b) ->
        case a of
          [c] -> c `compare` b
          _ ->
            case b of
              Arr' _ c -> length a `compare` length c
              Tup' c -> length a `compare` length c
              _ -> GT
      (_, Break) -> EQ
      (Break, _) -> EQ
      (Out' _ _, _) -> EQ
      (_, Out' _ _) -> EQ

-- data Signal = Break | Continue | Return

-- TODO: Make a tShow that converts directly to text later?
instance Show Value where
  show :: Value -> String
  show val =
    case val of
      Int' int -> show int
      Flt' flt -> show flt
      Chr' chr -> show chr
      Str' str -> T.unpack str
      Bln' bln -> show bln
      Pth' pth -> pth
      Typ' typ -> show typ
      Arr' _ x -> '[':(intercalate " " $ map show x)   ++ "]"
      Tup' tup -> '(':(intercalate " " $ map show tup) ++ ")"
      Break    -> "Break"
      Out' _ _ -> "Out"

-- Added a "break" type for sending the break signal to the for/while loop controller.
data Type =
  Tint | Tflt | Tchr | Tstr | Ttyp | Tbln | Tpth |
  Tarr Type | Ttup [Type] | Tany | Tbrk | Tout
  deriving (Ord, Eq)

instance Show Type where
  show :: Type -> String
  show typ =
    case typ of
      Tint -> "Int"
      Tflt -> "Flt"
      Tchr -> "Chr"
      Tstr -> "Str"
      Ttyp -> "Typ"
      Tbln -> "Bln"
      Tpth -> "Pth"
      Tany -> "Any"
      Tbrk -> "Brk"
      Tarr t -> "[" ++ show t ++ "]"
      Ttup t -> "(" ++ intercalate " " (map show t) ++ ")"
      Tout -> "Out"

hasTany :: Type -> Bool
hasTany typ =
  case typ of
    Tarr tin -> hasTany tin
    Ttup ts  -> or (map hasTany ts)
    Tany     -> True
    _        -> False

-- Used to handle compound types like [Int] or (Int Chr (Flt [Str]))
-- Can't used tok2typ for it.
compoundType :: Token -> Maybe Type
compoundType tok =
  case tok of
    Typ t  -> Just t
    Arr ts@(_:_) -> do
      typs <- sequence $ map compoundType ts
      Just $ Tarr $ tCollapse (Ttup typs)
    Tup ts@(_:_) -> do
      typs <- sequence $ map compoundType ts
      Just $ tCollapse (Ttup typs)
    _ -> Nothing

-- instance Eq Type where
--   (==) :: Type -> Type -> Bool
--   Tany == _ = True
--   _ == Tany = True
--   Tint == Tint = True
--   Tflt == Tflt = True
--   Tchr == Tchr = True
--   Tstr == Tstr = True
--   Ttyp == Ttyp = True
--   Tbln == Tbln = True
--   Tpth == Tpth = True
--   Tnon == Tnon = True
--   Tarr a == Tarr b = a == b
--   Ttup a == Ttup b = a == b
--   _ == _ = False

-- Base operators should just be handled by the evaluation module.
-- They can check the name.
-- Shit, how do I check the inputs to give back the correct operation?
-- The [String] is supposed to be the variable names.
-- How do I keep track of the scope the function is defined in?
-- Base contains a string for the name since we have to re-pass it to the interpreter.
-- Might turn it into an enum but... should I?
-- Shows output type.
-- Defined stores a scope so that it could only access the memory it has
data Operator =
  Base (Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))) Type |
  Defined Word8 (VarTree, Expression) Type

instance Eq Operator where
  (==) :: Operator -> Operator -> Bool
  Defined w1 (v1, e1) t1 == Defined w2 (v2, e2) t2 = and [w1 == w2, v1 == v2, e1 == e2, t1 == t2]
  _ == _ = False

-- Might not work as intended if a VarGroup contains only one VarName.
zipVar :: VarTree -> Value -> Maybe [(T.Text, Data)]
zipVar (VarName vn) val = Just [(vn, Val $ Right val)]
zipVar (VarGroup vg) val =
  case val of
    Tup' tup ->
      if length tup == length vg
      then
        case sequence $ zipWith (zipVar) vg tup of
          Nothing -> Nothing
          Just vv -> Just $ concat vv
      else Nothing
    _ -> Nothing

data VarTree = VarName T.Text | VarGroup [VarTree]
  deriving (Eq)

instance Show Operator where
  show :: Operator -> String
  show op =
    case op of
      Base _ t    -> "base with output type "    ++ show t
      Defined s (_, e) t -> "defined at " ++ show s ++ "with output type " ++ show t ++ " and Expression " ++ show e

insertOp :: (Type, Type) -> Operator -> OpMap -> OpMap
insertOp (left, right) op (lmap, lany) =
  if hasTany left
  then
    case lany of
      Just rside -> (lmap, Just $ insertRight rside)
      Nothing -> (lmap, Just $ insertRight (M.empty, Nothing))
  else
    case left `M.lookup` lmap of
      Just rside -> (M.insert left (insertRight rside) lmap, lany)
      Nothing -> (M.insert left (insertRight (M.empty, Nothing)) lmap, lany)
  where
    insertRight (rmap, rany) =
      if hasTany right
      then (rmap, Just op)
      else (M.insert right op rmap, rany)

lookupOp :: (Type, Type) -> OpMap -> Maybe Operator
lookupOp (left, right) (lmap, lany) =
  case left `M.lookup` lmap of
    Nothing ->
      case lany of
        Nothing -> Nothing
        Just rside -> lookupR rside
    Just rside -> lookupR rside
  where
    lookupR (rmap, rany) =
      case right `M.lookup` rmap of
        Just op -> Just op
        Nothing ->
          case lany of
            Nothing -> rany
            Just (lamap, anyany) ->
              case right `M.lookup` lamap of
                Nothing -> anyany
                Just op -> Just op

-- Error-checking should be done by the interpreter. This tree allows for variables.
data Expression = Expression Word8 T.Text Expression Expression | Operand Token
  deriving (Show, Ord, Eq)

flatten :: Expression -> [Token]
flatten (Operand o) = [o]
flatten (Expression p o l r) =
  case (l, r) of
    ((Operand (Tup [])), (Operand (Tup []))) -> [Opr p o]
    (_, (Operand (Tup []))) -> flatten l ++ [Opr p o]
    ((Operand (Tup [])), _) -> Opr p o:flatten r
    _ -> flatten l ++ Opr p o:flatten r

vFlatten :: Value -> [Value]
vFlatten v =
  case v of
    Tup' vs -> concat $ map vFlatten vs
    Arr' _ vs -> concat $ map vFlatten vs
    _ -> [v]

getMem :: Memory -> T.Text -> Maybe Data
getMem (x:xs) key =
  case M.lookup key x of
    Nothing -> getMem xs key
    result  -> result
getMem [] _ = Nothing

getTopOpMap :: Memory -> T.Text -> OpMap
getTopOpMap (m:_) opname =
  case M.lookup opname m of
    Just (Op _ opmap) -> opmap
    _ -> (M.empty, Nothing)
getTopOpMap [] _ = (M.empty, Nothing)
val2typ :: Value -> Type
val2typ val =
  case val of 
    Int' _     -> Tint
    Flt' _     -> Tflt
    Chr' _     -> Tchr
    Str' _     -> Tstr
    Bln' _     -> Tbln
    Pth' _     -> Tpth
    Typ' _     -> Ttyp
    Arr' typ _ -> Tarr typ
    Tup' tup   -> Ttup (map val2typ tup)
    Break      -> Tbrk
    Out' _ _   -> Tout

tok2typ :: Memory -> Token -> Type
tok2typ mem t =
  case t of
    Opr _ _ -> Tstr
    Str _   -> Tstr
    Pth _   -> Tpth
    Int _   -> Tint
    Flt _   -> Tflt
    Chr _   -> Tchr
    Bln _   -> Tbln
    Typ _   -> Ttyp
    Fmt _   -> Tstr
    Tup tup -> Ttup $ map (tok2typ mem) tup
    Arr arr -> Tarr $
      case headMaybe arr of
        Just h  -> tok2typ mem h
        Nothing -> Tany
    Var var ->
      case getMem mem var of
        Nothing  -> Tstr
        Just dat ->
          case dat of
            Op _ _  -> Tany
            Val (Right val) -> val2typ val
            Val (Left  typ) -> typ
    Wrd wrd ->
      case getMem mem wrd of
        Nothing  -> Tstr
        Just dat ->
          case dat of
            Op _ _  -> Tany
            Val (Right val) -> val2typ val
            Val (Left  typ) -> typ

-- (a) -> a repeatedly.
collapse :: Token -> Token
collapse (Tup [x]) = collapse x
collapse x           = x

tCollapse :: Type -> Type
tCollapse t =
  case t of
    Ttup [t'] -> tCollapse t'
    Ttup t'   -> Ttup $ map tCollapse t' 
    _         -> t

vtCollapse :: VarTree -> VarTree
vtCollapse vt =
  case vt of
    VarGroup [vn] -> vtCollapse vn
    VarGroup  vs  -> VarGroup (map vtCollapse vs)
    VarName   vn  -> VarName vn

vCollapse :: Value -> Value
vCollapse (Tup' [v]) = vCollapse v
vCollapse v =
  case v of
    Tup' vs   -> Tup'   (map vCollapse vs)
    Arr' t vs -> Arr' t (map vCollapse vs)
    _         -> v

hasOp :: Memory -> [Token] -> Bool
hasOp _ [] = False
hasOp mem (x:xs) =
  (
    case x of
      Opr _ _ -> True
      Wrd wrd ->
        case mem `getMem` wrd of
          Just (Op _ _) -> True
          _ -> False
      _ -> False
  )
  || hasOp mem xs

getRank :: Token -> Word8
getRank x =
  case x of
    Opr r _ -> r
    _       -> 255

-- This needs to be string because those things need strings.
argify :: Value -> [T.Text]
argify val =
  case val of
    Int' int -> [T.show int]
    Flt' flt -> [T.show flt]
    Chr' chr -> [T.singleton chr]
    Str' str -> [str]
    Bln' bln -> [T.show bln]
    Pth' pth -> [T.pack pth]
    Typ' typ -> [T.show typ]
    Tup' tup -> concat $ map argify tup
    Arr' _ arr -> concat $ map argify arr
    Break    -> []
    Out' _ _ -> [] 

-- Wanna make it better, but I don't know how.
-- argIO :: [Value] -> IO [T.Text]
-- argIO vals =
--   (
--     sequence $ map
--     (
--       \v ->
--         -- This doesn't let people do f"Hi {ls}", so maybe I'll add a cmd operator or something?
--         case vCollapse v of
--           Tup' (Str' _:_)   -> do
--             out <- cap v
--             case out of
--               Nothing -> return []
--               Just txt -> return txt
--           Tup' (Pth' _:_)   -> do
--             out <- cap v
--             case out of
--               Nothing -> return []
--               Just txt -> return txt
--           Arr' Tstr xs -> return (map T.show xs)
--           Arr' Tpth xs -> return (map T.show xs)
--           -- Arr' Tstr _ -> do
--           --   out <- cap v
--           --   case out of
--           --     Nothing -> return []
--           --     Just txt -> return (T.words txt)
--           -- Arr' Tpth _ -> do
--           --   out <- cap v
--           --   case out of
--           --     Nothing -> return []
--           --     Just txt -> return (T.words txt)
--           Tup' [] -> return []
--           _        -> return [T.show v]
--     )
--     vals
--   ) >>=
--   (\x -> return $ concat x)
--   where
--     cap :: Value -> IO (Maybe [T.Text])
--     cap v = do
--       o <- cmd v
--       case o of
--         Nothing -> return Nothing
--         Just (out, procHand) -> do
--           output <- do
--             -- hSetBuffering out NoBuffering
--             hSetBinaryMode out True
--             B.hGetContents out
--           -- copyHandleData out stdout
--           -- DEBUG
--           -- putStr output
--           exitCode <- waitForProcess procHand
--           if exitCode /= ExitSuccess
--           then T.putStrLn $ "Process " `T.append` T.show v `T.append` " exited with exit code: " `T.append` T.show exitCode
--           else return ()
--           -- Bash does this too. Might as well, since I don't want my args to contain newlines.
--           return $ Just (T.words $ T.decodeUtf8With T.lenientDecode output)
--   -- (
--   --   sequence $
--   --   map
--   --   (
--   --     \v ->
--   --       case v of
--   --         Tup' _   -> cap v
--   --         Arr' _ _ -> cap v
--   --         _        -> return $ Just $ show v
--   --   ) vals
--   -- ) >>=
--   -- (\outs -> return $ catMaybes outs)   Tup' tup -> concat $ map argify tup
-- -- This is exclusively for ",". The other pipers (<-, |, &, ->, =>)
-- -- use their own functions that either don't inherit some std files or are async.
-- -- Surrenders the program to an external process.
-- -- Returns data in its stdout to the interpreter.
cmd :: Value -> IO (Maybe (Handle, ProcessHandle))
cmd val =
  case vFlatten val of
    [Out' o h]              -> return $ Just (o, h)
    -- Tup' (command:args) -> exec command (Tup' args)
    -- Arr' _ (command:args) -> exec command (Tup' args)
    (Str' command:args)          -> exec (T.unpack command) (Tup' args)
    (Pth' command:args)          -> exec command (Tup' args)
    -- Pth' command          -> exec (Pth' command) []
    _ -> putStrLn (show val) >>= \_ -> return Nothing
  where
    handler :: IOError -> IO (Maybe (Handle, ProcessHandle))
    handler e =
      T.putStrLn ("cmd: " `T.append` T.show e) >>=
      \_ -> return Nothing
    exec command args = (exec' command args) `catch` handler
    exec' command args = cproc command (map T.unpack (argify args))
      -- case command of
      --   Tup' (x:xs) -> cproc (show x) (map show xs ++ map T.unpack (argify args))
      --   Arr' _ (x:xs) -> cproc (show x) (map show xs ++ map T.unpack (argify args))
      --   _ -> cproc (show command) (map T.unpack (argify args))
        -- Tup' _   -> do
        --   out <- cap command
        --   let args' = argify args
        --   case out of
        --     Nothing -> return Nothing
        --     Just out' -> cproc (T.unpack out') (map T.unpack args')
        -- Arr' _ _ -> do
        --   out <- cap command
        --   -- args' <- argIO args
        --   let args' = argify args
        --   case out of
        --     Nothing -> return Nothing
        --     Just out' -> cproc (T.unpack out') (map T.unpack args')
        -- _ ->
          -- (argIO args) >>=
          -- (\args' -> cproc (show command) (map T.unpack args'))
          -- cproc (show command) (map T.unpack (argify args))
    cproc :: String -> [String] -> IO (Maybe (Handle, ProcessHandle))
    cproc x xs = do
      hans <- createProcess (proc x xs) {std_out = CreatePipe}
      case hans of
        (_, Just o, _, h) -> return $ Just (o, h)
        _ -> return Nothing
    -- cap :: Value -> IO (Maybe T.Text)
    -- cap v = do
    --   o <- cmd v
    --   case o of
    --     Nothing -> return Nothing
    --     Just (out, procHand) -> do
    --       output <- do
    --         -- hSetBuffering out NoBuffering
    --         hSetBinaryMode out True
    --         B.hGetContents out
    --       -- copyHandleData out stdout
    --       -- DEBUG
    --       -- putStr output
    --       exitCode <- waitForProcess procHand
    --       if exitCode /= ExitSuccess
    --       then T.putStrLn $ "Process " `T.append` T.show v `T.append` " exited with exit code: " `T.append` T.show exitCode
    --       else return ()
    --       -- Bash does this too. Might as well, since I don't want my args to contain newlines.
    --       return $ Just (T.unwords $ T.words $ T.decodeUtf8With T.lenientDecode output)

-- cmdConc :: Value -> IO ()
-- cmdConc val =
--   case val of
--     Tup' (command:args) -> exec command args
--     Arr' _ (command:args) -> exec command args
--     Str' command          -> exec (Str' command) []
--     Pth' command          -> exec (Pth' command) []
--     _ -> putStrLn $ show val
--   where
--     handler :: IOError -> IO ()
--     handler e = putStrLn $ "cmd: " ++ show e
--     exec command args = (exec' command args) `catch` handler
--     exec' command args =
--       case command of
--         Tup' _   -> do
--           out <- cap command
--           args' <- argIO args
--           case out of
--             Nothing -> return ()
--             Just out' ->
--               spawnProcess (T.unpack out') (map T.unpack args') >>=
--               \_ -> return ()
--         Arr' _ _ -> do
--           out <- cap command
--           args' <- argIO args
--           case out of
--             Nothing -> return ()
--             Just out' ->
--               spawnProcess (T.unpack out') (map T.unpack args') >>=
--               \_ -> return ()
--         _ ->
--           (argIO args) >>=
--           (\args' ->
--             spawnProcess (show command) (map T.unpack args')) >>=
--             \_ -> return ()
      
-- -- For usage in parts where the output needs to be captured.
-- -- E.G: tuples that includes this verse in its return value.
-- -- Doesn't work with fastfetch (and probably some other programs) for some reason.
-- -- The reason seems encoding-related so I just converted it to binary, thus fucking some characters up.
-- cap :: Value -> IO (Maybe T.Text)
-- cap val =
--   case val of
--     Tup' (command:args) -> exec command args
--     Arr' _ (command:args) -> exec command args
--     Str' command          -> exec (Str' command) []
--     _ -> do
--       -- don't know about this...
--       -- putStrLn $ show val
--       return $ Just $ T.show val
--   where
--     handler :: IOError -> IO (Maybe a)
--     handler e = do
--         T.putStrLn $ "cap: " `T.append` T.show e
--         return Nothing
--     exec command args = (exec' command args) `catch` handler
--     exec' :: Value -> [Value] -> IO (Maybe T.Text)
--     exec' command args =
--       let
--         capbody c as = do
--           as' <- argIO as
--           process <-
--              -- Create process accepts:
--             createProcess
--             (
--               -- as
--               CreateProcess
--               (
--                 RawCommand (T.unpack c) (map T.unpack as')
--               )
--               -- cwd, env
--               Nothing Nothing
--               -- std files
--               Inherit CreatePipe Inherit
--               -- close fds, create group, delegate ctl-c,
--               -- detach console, create new console, new session
--               False False True False False False
--               -- child group, child user, use process jobs
--               -- (wait for the entire thing to finish before unblocking on Windows.)
--               Nothing Nothing True
--             )
--           case process of
--             (_, Just out, _, procHand) -> do
--               output <- do
--                 -- hSetBuffering out NoBuffering
--                 hSetBinaryMode out True
--                 B.hGetContents out
--               -- copyHandleData out stdout
--               -- DEBUG
--               -- putStr output
--               exitCode <- waitForProcess procHand
--               if exitCode /= ExitSuccess
--               then T.putStrLn $ "Process " `T.append` c `T.append` T.unwords as' `T.append` " exited with exit code: " `T.append` T.show exitCode
--               else return ()
--               -- Bash does this too. Might as well, since I don't want my args to contain newlines.
--               return $ Just (T.unwords $ T.words $ T.decodeUtf8Lenient output)
--             _ ->
--               (T.putStrLn $ "Can't create process " `T.append` (T.intercalate " " $ argify val) `T.append` " properly") >>=
--               (\_ -> return Nothing)
--       in
--       case command of
--         Tup' _   -> do
--           out <- cap command
--           case out of
--             Nothing -> return Nothing
--             Just out' -> capbody out' args
--         Arr' _ _ -> do
--           out <- cap command
--           case out of
--             Nothing -> return Nothing
--             Just out' -> capbody out' args
--         _ -> capbody (T.show command) args
--         -- capbody (show command) args
