module Tssl.Data where

import qualified Data.Map as M
import Data.Word(Word8)
import Data.List(intercalate)
import Misc
import System.Process
import System.IO
import System.Exit
-- import System.Environment
import Control.Exception

-- Accepts the name and maps it to the value.
-- Variables and operators use the same namespace.
-- Maps name -> value
-- list for scope, kept track of in main and altered in eval 
type Memory = [M.Map String Data]

newscope :: Memory -> Memory
newscope mem = M.empty:mem

insertMem :: String -> Data -> Memory -> Memory
insertMem dname dat (m:ms) =  (M.insert dname dat m):ms
insertMem dname dat [] =  [M.insert dname dat M.empty]

insertVar :: String -> Value -> Memory -> Memory
insertVar vname val (m:ms) = (M.insert vname (Val $ Right val) m):ms
insertVar vname val [] = [M.insert vname (Val $ Right val) M.empty]

updateVar :: String -> Value -> Memory -> Memory
updateVar vname val (m:ms) =
  case vname `M.lookup` m of
    Nothing -> m:(updateVar vname val ms)
    Just _  -> (M.insert vname (Val $ Right val) m):ms
updateVar _ _ [] = []

insertBlankVar :: String -> Type -> Memory -> Memory
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
  Wrd String | Var String | Opr Word8 String | Str String | Pth String | Int Integer | Chr Char | Bln Bool | Typ Type |
  Tup [Token] | Arr [Token] | Fmt [Either String Token]
  deriving (Eq, Ord)

instance Show Token where
  show :: Token -> String
  show t =
    case t of
      Wrd   wrd -> "\ESC[30m"    ++ wrd                            ++ "\ESC[0m"
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
  Arr' Type [Value] | Tup' [Value] | Break -- | Sig' Signal
  deriving (Ord, Eq)

-- data Signal = Break | Continue | Return

instance Show Value where
  show :: Value -> String
  show val =
    case val of
      Int' int -> show int
      Flt' flt -> show flt
      Chr' chr -> show chr
      Str' str -> str
      Bln' bln -> show bln
      Pth' pth -> pth
      Typ' typ -> show typ
      Arr' _ x -> '[':(intercalate " " $ map show x)   ++ "]"
      Tup' tup -> '(':(intercalate " " $ map show tup) ++ ")"
      Break    -> "Break"

-- Added a "break" type for sending the break signal to the for/while loop controller.
data Type =
  Tint | Tflt | Tchr | Tstr | Ttyp | Tbln | Tpth |
  Tarr Type | Ttup [Type] | Tany | Tbrk
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
    Arr ts -> do
      typs <- sequence $ map compoundType ts
      Just $ Tarr $ tCollapse (Ttup typs)
    Tup ts -> do
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
zipVar :: VarTree -> Value -> Maybe [(String, Data)]
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

data VarTree = VarName String | VarGroup [VarTree]
  deriving (Eq)
instance Show Operator where
  show :: Operator -> String
  show op =
    case op of
      Base _ t    -> "base with output type "    ++ show t
      Defined s _ t -> "defined at " ++ show s ++ "with output type " ++ show t

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
data Expression = Expression Word8 String Expression Expression | Operand Token
  deriving (Show, Ord, Eq)

getMem :: Memory -> String -> Maybe Data
getMem (x:xs) key =
  case M.lookup key x of
    Nothing -> getMem xs key
    result  -> result
getMem [] _ = Nothing

getTopOpMap :: Memory -> String -> OpMap
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

hasOp :: [Token] -> Bool
hasOp [] = False
hasOp ((Opr _ _):_) = True
hasOp (_:xs) = hasOp xs

getRank :: Token -> Word8
getRank x =
  case x of
    Opr r _ -> r
    _       -> 255

argify :: Value -> [String]
argify val =
  case val of
    Int' int -> [show int]
    Flt' flt -> [show flt]
    Chr' chr -> [[chr]]
    Str' str -> [str]
    Bln' bln -> [show bln]
    Pth' pth -> [pth]
    Typ' typ -> [show typ]
    Tup' tup -> concat $ map argify tup
    Arr' _ arr -> concat $ map argify arr
    Break    -> []

-- Wanna make it better, but I don't know how.
argIO :: [Value] -> IO [String]
argIO vals =
  (
    sequence $ map
    (
      \v ->
        -- This doesn't let people do f"Hi {ls}", so maybe I'll add a cmd operator or something?
        case vCollapse v of
          Tup' (Str' _:_)   -> do
            out <- cap v
            case out of
              Nothing -> return []
              Just txt -> return (words txt)
          Tup' (Pth' _:_)   -> do
            out <- cap v
            case out of
              Nothing -> return []
              Just txt -> return (words txt)
          Arr' Tstr _ -> do
            out <- cap v
            case out of
              Nothing -> return []
              Just txt -> return (words txt)
          Arr' Tpth _ -> do
            out <- cap v
            case out of
              Nothing -> return []
              Just txt -> return (words txt)
          Tup' [] -> return []
          _        -> return [show v]
    )
    vals
  ) >>=
  (\x -> return $ concat x)
  -- (
  --   sequence $
  --   map
  --   (
  --     \v ->
  --       case v of
  --         Tup' _   -> cap v
  --         Arr' _ _ -> cap v
  --         _        -> return $ Just $ show v
  --   ) vals
  -- ) >>=
  -- (\outs -> return $ catMaybes outs)   Tup' tup -> concat $ map argify tup
-- -- This is exclusively for ",". The other pipers (<-, |, &, ->, =>)
-- -- use their own functions that either don't inherit some std files or are async.
-- -- Surrenders the program to an external process.
-- -- Returns data in its stdout to the interpreter.
cmd :: Value -> IO ()
cmd val =
  case val of
    Tup' (command:args) -> exec command args
    Arr' _ (command:args) -> exec command args
    Str' command          -> exec (Str' command) []
    Pth' command          -> exec (Pth' command) []
    _ -> putStrLn $ show val
  where
    handler :: IOError -> IO ()
    handler e = putStrLn $ "cmd: " ++ show e
    exec command args = (exec' command args) `catch` handler
    exec' command args =
      case command of
        Tup' _   -> do
          out <- cap command
          args' <- argIO args
          case out of
            Nothing -> return ()
            Just out' -> callProcess out' args'
        Arr' _ _ -> do
          out <- cap command
          args' <- argIO args
          case out of
            Nothing -> return ()
            Just out' -> callProcess out' args'
        _ ->
          (argIO args) >>=
          (\args' -> callProcess (show command) args')

cmdConc :: Value -> IO ()
cmdConc val =
  case val of
    Tup' (command:args) -> exec command args
    Arr' _ (command:args) -> exec command args
    Str' command          -> exec (Str' command) []
    Pth' command          -> exec (Pth' command) []
    _ -> putStrLn $ show val
  where
    handler :: IOError -> IO ()
    handler e = putStrLn $ "cmd: " ++ show e
    exec command args = (exec' command args) `catch` handler
    exec' command args =
      case command of
        Tup' _   -> do
          out <- cap command
          args' <- argIO args
          case out of
            Nothing -> return ()
            Just out' ->
              spawnProcess out' args' >>=
              \_ -> return ()
        Arr' _ _ -> do
          out <- cap command
          args' <- argIO args
          case out of
            Nothing -> return ()
            Just out' ->
              spawnProcess out' args' >>=
              \_ -> return ()
        _ ->
          (argIO args) >>=
          (\args' ->
            spawnProcess (show command) args') >>=
            \_ -> return ()
      
-- For usage in parts where the output needs to be captured.
-- E.G: tuples that includes this verse in its return value.
-- Doesn't work with fastfetch (and probably some other programs) for some reason.
-- The reason seems encoding-related so I just converted it to binary, thus fucking some characters up.
cap :: Value -> IO (Maybe String)
cap val =
  case val of
    Tup' (command:args) -> exec command args
    Arr' _ (command:args) -> exec command args
    Str' command          -> exec (Str' command) []
    _ -> do
      -- don't know about this...
      -- putStrLn $ show val
      return $ Just $ show val
  where
    handler :: IOError -> IO (Maybe a)
    handler e = do
        putStrLn $ "cap: " ++ show e
        return Nothing
    exec command args = (exec' command args) `catch` handler
    exec' :: Value -> [Value] -> IO (Maybe String)
    exec' command args =
      let
        capbody c as = do
          as' <- argIO as
          process <-
             -- Create process accepts:
            createProcess
            (
              -- as
              CreateProcess
              (
                RawCommand c as'
              )
              -- cwd, env
              Nothing Nothing
              -- std files
              Inherit CreatePipe Inherit
              -- close fds, create group, delegate ctl-c,
              -- detach console, create new console, new session
              False False True False False False
              -- child group, child user, use process jobs
              -- (wait for the entire thing to finish before unblocking on Windows.)
              Nothing Nothing True
            )
          case process of
            (_, Just out, _, procHand) -> do
              output <- do
                hSetBuffering out NoBuffering
                hSetBinaryMode out True
                hGetContents out
              -- copyHandleData out stdout
              -- DEBUG
              -- putStr output
              exitCode <- waitForProcess procHand
              if exitCode /= ExitSuccess
              then putStrLn $ "Process " ++ c ++ unwords as' ++ " exited with exit code: " ++ show exitCode
              else return ()
              -- Bash does this too. Might as well, since I don't want my args to contain newlines.
              return $ Just (unwords $ words output)
            _ ->
              (putStrLn $ "Can't create process " ++ (intercalate " " $ argify val) ++ " properly") >>=
              (\_ -> return Nothing)
      in
      case command of
        Tup' _   -> do
          out <- cap command
          case out of
            Nothing -> return Nothing
            Just out' -> capbody out' args
        Arr' _ _ -> do
          out <- cap command
          case out of
            Nothing -> return Nothing
            Just out' -> capbody out' args
        _ -> capbody (show command) args
        -- capbody (show command) args
