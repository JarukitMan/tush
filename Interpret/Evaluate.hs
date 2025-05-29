module Interpret.Evaluate where
-- import Interpret.Token
import Interpret.Data
import Interpret.Parse
import qualified Data.Map as M
import Misc

-- Memory is a Map of Name -> Data
-- Data = Op Word8 Word8 (M.Map ([Type], [Type]) Operator) | Val Value
interpret :: Memory -> Expression -> IO (Maybe (Memory, Value))
interpret mem (Expression _ opr left right) =
  case getMem mem opr of
    Nothing -> do
      putStrLn $ "Operator " ++ opr ++ " not defined"
      return Nothing
    Just dat ->
      case dat of
        Val _ -> do
          putStrLn "Evaluation Error: Literal found at non-bottom level."
          return Nothing
        Op _ opmap -> do
          case exp2typs mem left of
            Nothing -> do
              putStrLn $ "Can't infer the type of operation " ++ opr ++ "'s left-hand side."
              return Nothing
            Just lefts ->
              case exp2typs mem right of
                Nothing -> do
                  putStrLn $ "Can't infer the type of operation " ++ opr ++ "'s right-hand side."
                  return Nothing
                Just rights ->
                  case (lefts, rights) `M.lookup` opmap of
                    Nothing -> do
                      putStrLn $
                        "Operator " ++ opr ++ " does not contain a variation that accepts type "
                        ++ show (exp2typs mem left) ++ " and " ++ show (exp2typs mem right)
                      return Nothing
                    Just (Base opfun _) -> opfun mem left right
                    Just (Defined scopenum (names, optree) _) -> do
                      Just (_, lefts') <- interpret mem left
                      Just (_, rights') <- interpret mem left
                      let
                        (used, unused) = splitAt (fromIntegral scopenum) mem
                        vals =
                          case lefts' of
                            Tup' tl ->
                              case rights' of
                                Tup' tr -> tl ++ tr
                                r       -> tl ++ [r]
                            l ->
                              case rights' of
                                Tup' tr -> l:tr
                                r       -> l:r:[]
                        newmem = M.fromList (zip names (map Val vals)):used
                      Just (frontmem, result) <- interpret newmem optree
                      return $ Just (frontmem ++ unused, result)
interpret mem (Operand x) =
  case x of
    Var var ->
      case getMem mem var of
        Just dat ->
          case dat of
            Val val  -> return $ Just (mem, val)
            Op _ _ -> do
              putStrLn  $ "Evaluation Error: Operand " ++ var ++ " stored as a variable name."
              return Nothing
        Nothing -> do
          putStrLn  $ "Variable " ++ var ++ " not found in scope."
          return Nothing
    Opr _ opr -> do
      putStrLn  $ "Evaluation Error: Operand " ++ opr ++ "found at bottom level."
      return Nothing
    Int int -> return $ Just (mem, Int' int)
    Chr chr -> return $ Just (mem, Chr' chr)
    Str str -> return $ Just (mem, Str' str)
    Bln bln -> return $ Just (mem, Bln' bln)
    Pth pth -> return $ Just (mem, Pth' pth)
    Typ typ -> return $ Just (mem, Typ' typ)
    -- Type-checking done here.
    Arr arr   -> do
      ar <- handletup arr
      case ar of
        Nothing -> do
          putStrLn $ "Array " ++ show arr ++ " cannot be evaluated."
          return Nothing
        Just arr' -> do
          let arrtypes = map val2typ arr' 
          if dupes arrtypes
          then
            case headMaybe arrtypes of
              Just typ -> return $ Just (mem, Arr' typ  arr')
              Nothing  -> return $ Just (mem, Arr' Tany arr') 
          else do
            putStrLn $ "Array " ++ show arr ++ "member types do not match."
            return Nothing
    Tup tup   -> do
      tp <- handletup tup
      case tp of
        Nothing   -> do
          putStrLn $ "Tuple " ++ show tup ++ " cannot be evaluated."
          return Nothing
        Just tup' -> return $ Just (mem, Tup' tup')
    Fmt fmt   ->
      return $ Just
      (
        mem, Fmt' $ map
        (
          \part ->
          case part of
            Left str  -> Left str
            Right tok -> Right $ parse [tok]
        )
        fmt
      ) -- Right (mem, Fmt' fmt)
    where
      handletup :: [Token] -> IO (Maybe [Value])
      -- FIXME: seems to loop infinitely for some reason.
      handletup _ = return $ Just [Int' 10]
      -- handletup xs = do
      --   putStrLn $ "handling " ++ show xs
      --   tup <- ((interpret $ newscope mem) . parse) xs
      --   case tup of
      --     Nothing -> return Nothing
      --     Just (_, Tup' tup') -> return $ Just tup'
      --     Just (_, val)       -> return $ Just [val]
--   Var String | Opr Word8 String | Str String | Pth String | Int Integer | Chr Char | Bln Bool | Typ Type |
--   Tup [Token] | Arr [Token] | Fmt [Either String Token]
--   deriving (Eq, Ord, Show)

-- -- Strict form of token.
-- data Value =
--   Int' Integer | Flt' Double | Chr' Char | Str' String | Bln' Bool | Pth' String | Typ' Type |
--   Arr' Type [Value] | Tup' [Value]| Fmt' [Either Expression String] | None 
