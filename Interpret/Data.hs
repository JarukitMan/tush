module Interpret.Data where

import qualified Data.Map as M
import Data.Word(Word8)
import Data.List(intercalate)
import Misc

-- Accepts the name and maps it to the value.
-- Variables and operators use the same namespace.
-- Maps name -> value
-- list for scope, kept track of in main and altered in eval 
type Memory = [(M.Map String Data)]

newscope :: Memory -> Memory
newscope mem = M.empty:mem

-- Op in the Value enumuration returns the cardinality, scope and
-- a map from input types to the operand names, body, and output type.
-- This is done so that one single namespace is used
-- for both variables and operators while allowing overloading. 
-- Op is rank, map from types to body
data Data = Op Word8 (M.Map ([Type], [Type]) Operator) | Val Value
  deriving (Show)

data Token =
  Var String | Opr Word8 String | Str String | Pth String | Int Integer | Chr Char | Bln Bool | Typ Type |
  Tup [Token] | Arr [Token] | Fmt [Either String Token]
  deriving (Eq, Ord)

instance Show Token where
  show :: Token -> String
  show t =
    case t of
      Var   var -> "\ESC[30m"    ++ var                            ++ "\ESC[0m"
      Opr _ opr -> "\ESC[31m"    ++ opr                            ++ "\ESC[0m"
      Str   str -> "\ESC[32m\""  ++ str                            ++ "\"\ESC[0m"
      Pth   pth -> "\ESC[33m"    ++ pth                            ++ "\ESC[0m"
      Int   int -> "\ESC[34m"    ++ show int                       ++ "\ESC[0m"
      Chr   chr -> "\ESC[35m'"   ++ chr                            :  "'\ESC[0m"
      Bln   bln -> "\ESC[36m"    ++ show bln                       ++ "\ESC[0m"
      Typ   typ -> "\ESC[37m"    ++ show typ                       ++ "\ESC[0m"
      Tup   tup -> "\ESC[38m("   ++ intercalate " " (map show tup) ++ ")\ESC[0m"
      Arr   arr -> "\ESC[39m["   ++ intercalate " " (map show arr) ++ "]\ESC[0m"
      Fmt   fmt ->
        "\ESC[32mf\""
        ++ intercalate " "
        (map (\x -> case x of {
        Left str -> "\ESC[32m" ++ str ++ "\ESC[0m" ;
        Right tup -> show tup}) fmt) ++
        "\ESC[32m\"\ESC[0m"

-- data Word =
--   Var String | Opr Word8 String | Str String | Pth String | Int Integer | Chr Char | Bln Bool | Typ Type |
--   Tup Expression | Arr Expression | Fmt [Either String Expression]
--   deriving (Eq, Ord, Show)

-- Strict form of token.
data Value =
  Int' Integer | Flt' Double | Chr' Char | Str' String | Bln' Bool | Pth' String | Typ' Type |
  Arr' Type [Value] | Tup' [Value]| Fmt' [Either String Expression] | None 
  deriving (Show, Ord, Eq)

data Type =
  Tint | Tflt | Tchr | Tstr | Ttyp | Tbln | Tpth |
  Tarr Type | Ttup [Type] | Tnon | Tany | Tfmt
  deriving (Show, Ord, Eq)

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
  Base (Memory -> Expression -> Expression -> IO (Maybe (Memory, Value))) Type |
  Defined Word8 ([String], Expression) Type

instance Show Operator where
  show :: Operator -> String
  show op =
    case op of
      Base _ t    -> "base with output type "    ++ show t
      Defined s _ t -> "defined at " ++ show s ++ "with output type " ++ show t

-- Error-checking should be done by the interpreter. This tree allows for variables.
data Expression = Expression Word8 String Expression Expression | Operand Token
  deriving (Show, Ord, Eq)

getMem :: Memory -> String -> Maybe Data
getMem [] _ = Nothing
getMem (x:xs) key =
  case M.lookup key x of
    Nothing -> getMem xs key
    result  -> result

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
    Fmt' _     -> Tfmt
    Arr' typ _ -> Tarr typ
    Tup' tup   -> Ttup (map val2typ tup)
    None       -> Tnon

tok2typ :: Memory -> Token -> Type
tok2typ mem t =
  case t of
    Opr _ _ -> Tstr
    Str _   -> Tstr
    Pth _   -> Tpth
    Int _   -> Tint
    Chr _   -> Tchr
    Bln _   -> Tbln
    Typ _   -> Ttyp
    Fmt _   -> Tfmt
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
            Val val -> val2typ val

-- This is "light interpretation" for type-checking functions.
-- Also used for operator input matching.
exp2typs :: Memory -> Expression -> Maybe [Type]
exp2typs mem (Operand x) = Just $ tCollapse [tok2typ mem x]
exp2typs mem (Expression _ op left right) = do
  dat <- getMem mem op
  case dat of
    Val _ -> Nothing
    Op _ opmap -> do
      lefts <- exp2typs mem left
      rights <- exp2typs mem right
      opr <- M.lookup (lefts, rights) opmap
      case opr of
        Base _ t -> Just $ tCollapse [t]
        Defined _ _ t -> Just $ tCollapse [t]

-- (a) -> a repeatedly.
collapse :: Token -> Token
collapse (Tup [x]) = collapse x
collapse x           = x

tCollapse :: [Type] -> [Type]
tCollapse t =
  case t of
    [Ttup t'] -> tCollapse t' 
    _ -> t

getRank :: Token -> Word8
getRank x =
  case x of
    Opr r _ -> r
    _       -> 255

-- tok2val :: Memory -> Token -> Either String Value
-- tok2val mem x =
--   case x of
--     Int num -> Right $ Int' num
--     Chr chr -> Right $ Chr' chr
--     Bln bln -> Right $ Bln' bln
--     Str str -> Right $ Str' str
--     Pth pth -> Right $ Pth' pth
--     Typ typ -> Right $ Typ' typ
--     Tup _   -> undefined
--     Arr _   -> undefined
--     Fmt _   -> undefined
--     Var v   ->
--       case getMem mem v of
--         Just (Val lit) -> Right $ lit
--         _   -> Left  $ "Identifier not found in scope: " ++ v
--     Opr _ _ -> Left "Parse Error: An operation appeared at the bottom level."
