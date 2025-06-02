module Interpret.Evaluate where
-- import Interpret.Token
import Interpret.Data
import Interpret.Parse
import qualified Data.Map as M
import Misc

-- Memory is a Map of Name -> Data
--                         v seems like that to the outside
-- Data = Op Word8 Word8 (M.Map ([Type], [Type]) Operator) | Val Value
-- return will probably be hacked into the "," operator.
-- This means it needs to know the return type of the tuple it's in.
-- So THAT will be put into the interpret function.
interpret :: Type -> Memory -> Expression -> IO (Maybe (Memory, Value))
interpret t mem (Expression _ opr left right) =
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
          case exp2typs mem left of
            Nothing -> do
              putStrLn $ "Can't infer the type of operation `" ++ opr ++ "`'s left-hand side."
              return Nothing
            Just lefts ->
              case exp2typs mem right of
                Nothing -> do
                  putStrLn $ "Can't infer the type of operation `" ++ opr ++ "`'s right-hand side."
                  return Nothing
                Just rights ->
                  case (lefts, rights) `lookupOp` opmap of
                    Nothing -> do
                      putStrLn $
                        "Operator `" ++ opr ++ "` does not contain a variation that accepts type "
                        ++ show (exp2typs mem left) ++ " and " ++ show (exp2typs mem right)
                      return Nothing
                    Just (Base opfun _) -> opfun mem left right
                    Just (Defined scopenum (names, optree) _) -> do
                      Just (_, lefts') <- interpret t mem left
                      Just (_, rights') <- interpret t mem left
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
                      Just (frontmem, result) <- interpret t newmem optree
                      return $ Just (frontmem ++ unused, result)
interpret _ mem (Operand x) =
  case x of
    Var var ->
      case getMem mem var of
        Just dat ->
          case dat of
            Val val  -> return $ Just (mem, val)
            Op _ _ -> do
              putStrLn  $ "Evaluation Error: Operand `" ++ var ++ "` stored as a variable name."
              return Nothing
        Nothing -> do
          putStrLn  $ "Variable `" ++ var ++ "` not found in scope."
          return Nothing
    Opr _ opr -> do
      putStrLn  $ "Evaluation Error: Operand `" ++ opr ++ "`found at bottom level."
      return Nothing
    Int int -> return $ Just (mem, Int' int)
    Chr chr -> return $ Just (mem, Chr' chr)
    Str str -> return $ Just (mem, Str' str)
    Bln bln -> return $ Just (mem, Bln' bln)
    Pth pth -> return $ Just (mem, Pth' pth)
    Typ typ -> return $ Just (mem, Typ' typ)
    Tup [ ] -> return $ Just (mem, Tup' [ ])
    -- Type-checking done here.
    Arr arr   -> do
      ar <- handletup arr
      case ar of
        Nothing -> do
          putStrLn $ "Array `" ++ show arr ++ "` cannot be evaluated."
          return Nothing
        Just (mem', arr') -> do
          let arrtypes = map val2typ arr' 
          if dupes arrtypes
          then
            case headMaybe arrtypes of
              Just typ -> return $ Just (mem', Arr' typ  arr')
              Nothing  -> return $ Just (mem', Arr' Tany arr') 
          else do
            putStrLn $ "Array `" ++ show arr ++ "`'s member types do not match."
            return Nothing
    Tup tup   -> do
      tp <- handletup tup
      case tp of
        Nothing   -> do
          putStrLn $ "Tuple `" ++ show tup ++ "` cannot be evaluated."
          return Nothing
        Just (mem', tup') -> return $ Just (mem', Tup' tup')
    Fmt fmt   -> do
      fm <- handletup $ map (\a -> case a of {Left txt -> Tup [Str txt]; Right xpr -> xpr}) fmt
      case fm of
        Nothing   -> do
          putStrLn $ "Formatted String `" ++ show fmt ++ "` cannot be evaluated."
          return Nothing
        Just (mem', fmt') -> return $ Just (mem', Str' $ concat $ map show fmt')
      -- return $ Just
      -- (
      --   mem, Fmt' $ map
      --   (
      --     \part ->
      --     case part of
      --       Left str  -> Left str
      --       Right tok -> Right $ parse [tok]
      --   )
      --   fmt
      -- ) -- Right (mem, Fmt' fmt)
    where
      -- handletup :: [Token] -> IO (Maybe [Value])
      -- handletup xs =
      --   let
      --     hasOp (Opr _ _:_) = True
      --     hasOp (_:xs') = hasOp xs'
      --     hasOp [] = False
      --   in
      --     if hasOp xs
      --     then do
      --       -- DEBUG
      --       putStrLn $ "handling " ++ show xs
      --       -- tup <- ((interpret t $ newscope mem) . parse) xs
      --       tup <-
      --         case exp2typs mem (parse xs) of
      --           Nothing -> return Nothing
      --           Just typs -> do
      --             -- DEBUG
      --             putStrLn $ "Output type is: " ++ show typs
      --             ((interpret (Ttup typs) $ newscope mem) . parse) xs
      --       case tup of
      --         Nothing -> return Nothing
      --         Just (_, Tup' tup') -> return $ Just tup'
      --         Just (_, val)       -> return $ Just [val]
      --     else do
      --       thing <-
      --         (
      --           sequence $ map
      --           (
      --             \typ ->
      --             case typ of
      --               Opr _ _ -> return Nothing
      --               Var var -> return $
      --                 case getMem mem var of
      --                   Nothing -> Nothing
      --                   Just (Op _ _) -> Nothing
      --                   Just (Val val) -> Just val
      --               Tup tup -> do
      --                 Just (_, result) <- case exp2typs mem (parse tup) of
      --                   Nothing -> return Nothing
      --                   Just typs -> ((interpret (Ttup typs) $ newscope mem) . parse) tup
      --                 return $ Just result
      --               tok -> do
      --                 -- Just (_, result) <- ((interpret t $ newscope mem) . parse) [tok]
      --                 Just (_, result) <- case exp2typs mem (parse [tok]) of
      --                   Nothing -> return Nothing
      --                   Just typs -> ((interpret (Ttup typs) $ newscope mem) . parse) [tok]
      --                 return $ Just result
      --           )
      --           xs
      --         )
      --       return $ sequence thing
      handletup :: [Token] -> IO (Maybe (Memory, [Value]))
      handletup xs =
        let
          hasOp (Opr _ _:_) = True
          hasOp (_:xs') = hasOp xs'
          hasOp [] = False
        in
          if hasOp xs
          then do
            -- DEBUG
            putStrLn $ "handling " ++ show xs
            -- tup <- ((interpret t $ newscope mem) . parse) xs
            tup <-
              case exp2typs mem (parse xs) of
                Nothing -> return Nothing
                Just typs -> do
                  -- DEBUG
                  putStrLn $ "Output type is: " ++ show typs
                  ((interpret (Ttup typs) $ newscope mem) . parse) xs
            case tup of
              Nothing -> return Nothing
              Just (_, Tup' tup') -> return $ Just (mem, tup')
              Just (_, val)       -> return $ Just (mem, [val])
          else
            -- This looks like a mess!
            foldl'
            (
              \acc t -> do
              acc' <- acc
              case acc' of
                Nothing -> return Nothing
                Just (m, ys) ->
                  case t of
                    Opr _ _ -> return Nothing
                    Var var ->
                      case getMem m var of
                        Nothing -> return Nothing
                        Just (Op _ _) -> return Nothing
                        Just (Val val) -> return $ Just (m, val:ys)
                    Tup tup -> do
                      out <-
                        case exp2typs m (parse tup) of
                          Nothing -> return Nothing
                          Just typs -> ((interpret (Ttup typs) $ newscope m) . parse) tup
                      case out of
                        Nothing -> return Nothing
                        Just (m', result) ->
                          return $ Just (m', result:ys)
                    tok -> do
                      -- Just (_, result) <- ((interpret t $ newscope mem) . parse) [tok]
                      out <-
                        case exp2typs m (parse [tok]) of
                          Nothing -> return Nothing
                          Just typs -> ((interpret (Ttup typs) $ newscope m) . parse) [tok]
                      case out of
                        Nothing -> return Nothing
                        Just (m', result) ->
                          return $ Just (m', result:ys)
            )
            (return $ Just (mem, []))
            xs
                
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
exp2typs :: Memory -> Expression -> Maybe [Type]
exp2typs mem (Operand x) =
  case x of
    Tup [ ] -> Just []
    Tup tup -> exp2typs mem (parse tup)
    -- Might want to do better...
    Arr arr ->
      case exp2typs mem (parse arr) of
        Nothing -> Nothing
        Just typs ->
          case headMaybe typs of
            Nothing -> Nothing
            Just thead -> Just [thead]
    tok -> Just [tok2typ mem tok]
exp2typs mem ex@(Expression _ op left right) =
  case findName "return" ex of
    Just (Expression _ _ _ r) -> exp2typs mem r
    Just x@(Operand _) -> exp2typs mem x
    Nothing -> do
      dat <- getMem mem op
      case dat of
        Val _ -> Nothing
        Op _ opmap -> do
          lefts <- exp2typs mem left
          rights <- exp2typs mem right
          opr <- (lefts, rights) `lookupOp` opmap
          case opr of
            Base _ t -> Just $ tCollapse [t]
            Defined _ _ t -> Just $ tCollapse [t]

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
