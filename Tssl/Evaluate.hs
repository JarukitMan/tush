module Tssl.Evaluate where
-- import Tssl.Token
import Tssl.Data
import Tssl.Parse
import System.Directory
import qualified Data.Map as M
import Misc
-- import System.Directory
-- import System.IO

-- Memory is a Map of Name -> Data
--                         v seems like that to the outside
-- Data = Op Word8 Word8 (M.Map ([Type], [Type]) Operator) | Val Value
-- return will probably be hacked into the "," operator.
-- This means it needs to know the return type of the tuple it's in.
-- So THAT will be put into the interpret function.
interpret :: Bool -> Type -> Memory -> Expression -> IO (Maybe (Bool, Memory, Value))
interpret b t mem (Expression _ opr left right) =
  case getMem mem opr of
    Nothing -> do
      putStrLn $ "Operator `" ++ opr ++ "` not defined"
      return Nothing
    Just dat ->
      case dat of
        Val _ -> do
          putStrLn "Evaluation Error: Literal found at non-bottom level."
          return Nothing
        Op _ opmap -> do
          case exp2typ mem left of
            Nothing -> do
              putStrLn $ "Can't infer the type of operation `" ++ opr ++ "`'s left-hand side at `" ++ show left ++ show opr ++ show right ++ "`."
              return Nothing
            Just lefts ->
              case exp2typ mem right of
                Nothing -> do
                  putStrLn $ "Can't infer the type of operation `" ++ opr ++ "`'s right-hand side at `" ++ show left ++ show opr ++ show right ++ "`."
                  return Nothing
                Just rights ->
                  case (lefts, rights) `lookupOp` opmap of
                    Nothing -> do
                      putStrLn $
                        "Operator `" ++ opr ++ "` does not contain a variation that accepts type "
                        ++ show lefts ++ " and " ++ show rights
                      return Nothing
                    Just (Base opfun _) -> opfun b t mem left right
                    Just (Defined scopenum (names, optree) _) -> do
                      Just (b', mem', lefts') <- interpret b t mem left
                      Just (b'', mem'', rights') <- interpret b' t mem' right
                      let
                        (used, unused) = splitAt (fromIntegral scopenum) mem''
                        -- Input Values.
                        vals = Tup' [vCollapse lefts', vCollapse rights']
                        newmem = case zipVar names vals of
                          Just ns -> Just $ (M.fromList ns):used
                          Nothing -> Nothing
                      case newmem of
                        Just nm -> do
                          out <- interpret b'' t nm optree
                          case out of
                            Nothing -> return Nothing
                            -- Simple memory dropping. Might not work as intended.
                            Just (nb, frontmem, result) -> return $ Just (nb, drop 1 frontmem ++ unused, result)
                        Nothing -> return Nothing
interpret b _ mem (Operand x) =
  case x of
    Var var ->
      case getMem mem var of
        Just dat ->
          case dat of
            Val (Right val) -> return $ Just (b, mem, val)
            Val (Left  typ) -> do
              putStrLn $ "Variable of type `" ++ show typ ++ "` does not have a value assigned to it."
              return Nothing
            Op _ _ -> do
              putStrLn  $ "Evaluation Error: Operand `" ++ var ++ "` stored as a variable name."
              return Nothing
        Nothing -> do
          putStrLn  $ "Variable `" ++ var ++ "` not found in scope."
          return Nothing
    Opr _ opr -> do
      putStrLn  $ "Evaluation Error: Operand `" ++ opr ++ "`found at bottom level."
      return Nothing
    Int int -> return $ Just (b, mem, Int' int)
    Chr chr -> return $ Just (b, mem, Chr' chr)
    Str str -> return $ Just (b, mem, Str' str)
    Bln bln -> return $ Just (b, mem, Bln' bln)
    Pth pth ->
      case pth of
        ('~':rest) -> do
          home <- getHomeDirectory
          return $ Just (b, mem, Pth' $ home ++ rest)
        _ -> do
          canon <- canonicalizePath pth
          return $ Just (b, mem, Pth' canon)
    Typ typ -> return $ Just (b, mem, Typ' typ)
    Tup [ ] -> return $ Just (b, mem, Tup' [ ])
    -- Type-checking done here.
    Arr arr   -> do
      ar <- handletup arr
      case ar of
        Nothing -> do
          putStrLn $ "Array `" ++ show x ++ "` cannot be evaluated."
          return Nothing
        Just (b', mem', arr') -> do
          let arrtypes = map val2typ arr' 
          if dupes arrtypes
          then
            case headMaybe arrtypes of
              Just typ -> return $ Just (b', mem', Arr' typ  arr')
              Nothing  -> return $ Just (b', mem', Arr' Tany arr') 
          else do
            putStrLn $ "Array `" ++ show x ++ "`'s member types do not match."
            return Nothing
    Tup tup   -> do
      tp <- handletup tup
      case tp of
        Nothing   -> do
          putStrLn $ "Tuple `" ++ show x ++ "` cannot be evaluated."
          return Nothing
        Just (b', mem', tup') -> return $ Just (b', mem', Tup' tup')
    Fmt fmt   -> do
      fm <- handletup $ map (\a -> case a of {Left txt -> Tup [Str txt]; Right xpr -> xpr}) fmt
      case fm of
        Nothing   -> do
          putStrLn $ "Formatted String `" ++ show x ++ "` cannot be evaluated."
          return Nothing
        -- Just (mem', fmt') -> return $ Just (mem', Str' $ concat $ map show fmt')
        Just (b', mem', fmt') -> do
          fmt'' <- argIO fmt'
          return $ Just (b', mem', Str' $ concat $ fmt'')
    where
      handletup :: [Token] -> IO (Maybe (Bool, Memory, [Value]))
      handletup xs =
          if hasOp xs
          then do
            -- DEBUG
            putStrLn $ "handling " ++ show xs
            -- tup <- ((interpret t $ newscope mem) . parse) xs
            tup <-
              case exp2typ mem (parse xs) of
                Nothing -> do
                  putStrLn $ "exp2typs failed to parse " ++ show xs ++ ". (before interpreting.)"
                  return Nothing
                Just typ -> do
                  -- DEBUG
                  putStrLn $ "Output type is: " ++ show typ
                  ((interpret b typ $ newscope mem) . parse) xs
            case tup of
              Nothing -> do
                putStrLn $ "exp2typs failed to parse " ++ show xs ++ "."
                return Nothing
              Just (b', _, Tup' tup') -> return $ Just (b', mem, (map vCollapse tup'))
              Just (b', _, val)       -> return $ Just (b', mem, [val])
          else do
            -- This looks like a mess!
            let
              resultReversed =
                foldl'
                (
                  \acc t -> do
                  acc' <- acc
                  case acc' of
                    Nothing -> return Nothing
                    Just (gb, m, ys) ->
                      case t of
                        Opr _ opr -> do
                          putStrLn $ "Found operator " ++ opr ++ " where it's not supposed to be (after hasOp returns False.)"
                          return Nothing
                        Var var ->
                          case getMem m var of
                            Nothing -> do
                              putStrLn $ "Found variable " ++ var ++ ", which does not exist."
                              return Nothing
                            Just (Op _ _) -> do
                              putStrLn $ "Found operator " ++ var ++ " where it's not supposed to be (after hasOp returns False.)"
                              return Nothing
                            Just (Val val) -> return $ Just (gb, m, val:ys)
                        Tup tup -> do
                          out <-
                            case exp2typ m (parse tup) of
                              Nothing -> do
                                putStrLn $ "exp2typs failed to parse " ++ show xs ++ ". (Nested)"
                                return Nothing
                              Just typ -> ((interpret gb typ $ newscope m) . parse) tup
                          case out of
                            Nothing -> return Nothing
                            Just (gb', m', result) ->
                              return $ Just (gb', m', (Right $ vCollapse result):ys)
                        tok -> do
                          -- Just (_, result) <- ((interpret t $ newscope mem) . parse) [tok]
                          out <-
                            case exp2typ m (parse [tok]) of
                              Nothing -> return Nothing
                              Just typ -> ((interpret gb typ $ newscope m) . parse) [tok]
                          case out of
                            Nothing -> return Nothing
                            Just (gb', m', result) ->
                              return $ Just (gb', m', Right result:ys)
                )
                (return $ Just (b, mem, []))
                xs
            resultRev <- resultReversed
            case resultRev of
              Nothing -> return Nothing
              Just (b', mem', resultR) ->
                case sequence resultR of
                  Right resultRightR -> return $ Just (b', mem', reverse resultRightR)
                  Left  errtype      -> do
                    putStrLn $ "Some variable in tuple `" ++ show xs ++ "` of type `" ++ show errtype ++ "` does not contain a value."
                    return Nothing
                
            -- return $ sequence thing
            
--   Var String | Opr Word8 String | Str String | Pth String | Int Integer | Chr Char | Bln Bool | Typ Type |
--   Tup [Token] | Arr [Token] | Fmt [Either String Token]
--   deriving (Eq, Ord, Show)

-- -- Strict form of token.
-- data Value =
--   Int' Integer | Flt' Double | Chr' Char | Str' String | Bln' Bool | Pth' String | Typ' Type |
--   Arr' Type [Value] | Tup' [Value]| Fmt' [Either Expression String] | None 

-- This is "light interpretation" for type-checking functions.
-- Also used for operator input matching.
-- This assumes that you CAN'T have operations lingering around in operands.
-- Which shouldn't happen. But in the case that it does happen, this would screw up.
-- There's something wrong inside that propagates a Nothing.
exp2typ :: Memory -> Expression -> Maybe Type
exp2typ mem (Operand x) =
  case collapse x of
    Tup [ ] -> Just $ Ttup []
    -- Tup [a] -> Just [tok2typ mem a]
    Tup tup ->
      if hasOp tup
      then exp2typ mem (parse tup)
      else Just $ tCollapse $ Ttup $ map (tok2typ mem) tup
    -- Might want to do better...
    Arr arr ->
      if hasOp arr
      then
        case exp2typ mem (parse arr) of
          Nothing           -> Nothing
          Just (Ttup (h:_)) -> Just $ Tarr $ tCollapse h
          Just h            -> Just $ Tarr $ tCollapse h
      else
        case headMaybe $ map (tok2typ mem) arr of
          Nothing -> Nothing
          Just h  -> Just $ Tarr $ tCollapse h
    tok -> Just $ tCollapse (tok2typ mem tok)
exp2typ mem ex@(Expression _ op left right) =
  case findName "return" ex of
    Just (Expression _ _ _ r) -> exp2typ mem r
    Just x@(Operand _) -> exp2typ mem x
    Nothing -> do
      dat <- getMem mem op
      case dat of
        Val _ -> Nothing
        Op _ opmap -> do
          l <- exp2typ mem left
          r <- exp2typ mem right
          opr <- (tCollapse l, tCollapse r) `lookupOp` opmap
          case opr of
            Base _ t -> Just $ tCollapse t
            Defined _ _ t -> Just $ tCollapse t

findName :: String -> Expression -> Maybe Expression
findName name ex@(Expression _ op left right) =
  if op == name
  then Just ex
  else
    case findName name left of
      Just lx -> Just lx
      Nothing -> findName name right
findName name word@(Operand x) =
  case x of
    Var var ->
      if var == name
      then Just word
      else Nothing
    Str str ->
      if str == name
      then Just word
      else Nothing
    _ -> Nothing
