module Interpret.Operators where

import Misc
import Interpret.Data
import Interpret.Evaluate
import Interpret.Parse
import Data.Maybe
import Data.List
import qualified Data.Char as C
import System.Directory
-- import System.Directory.Internal
-- import System.IO.Error
import System.Process
import System.IO
import System.Exit
-- import System.Environment
import Control.Exception
import GHC.Float
import Text.Read

add :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
add gbs typ mem lhs rhs = do
  left  <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          let
            addv x y =
              case (x, y) of
                (Int' a, Int' b)       -> Int' $ a + b
                (Int' a, Flt' b)       -> Flt' $ (fromInteger a) + b
                (Flt' a, Int' b)       -> Flt' $ a + (fromInteger b)
                (Flt' a, Flt' b)       -> Flt' $ a + b
                (Int' a, Chr' b)       -> Int' $ a + (toInteger $ C.ord b)
                (Chr' a, Int' b)       -> Chr' $ C.chr $ (C.ord a) + (fromInteger b)
                (Chr' a, Chr' b)       -> Chr' $ C.chr $ (C.ord a) + (C.ord b)
                (Bln' a, Bln' b)       -> Bln' $ a || b
                (Pth' a, Str' b)       -> Pth' $ a ++ ('/':b)
                (Str' a, b)            -> Str' $ a ++ show b
                (a, Str' b)            -> Str' $ (show a) ++ b
                (Arr' t1 a, Arr' t2 b) ->
                  if t1 == t2
                  then Arr' t1 (a ++ b)
                  else Tup' [Arr' t1 a, Arr' t2 b]
                (Arr' t a, b)          -> Arr' t $ map (addv b) a
                (a, Arr' _ b)          -> foldl' (addv) a b
                (Tup' a, b)            -> Tup'   $ a ++ [b]
                (a, Tup' b)            -> Tup'   $ a:b
                (a, b)                 -> Tup'     [a, b]
          in
            case (l, r) of
              (Pth' a, Int' b) -> do
                let dir = a ++ "/.."
                exists <- doesPathExist dir
                if exists
                then do
                  files <- listDirectory dir
                  let fcycle = cycle files
                  return $ Just (rbs, mr, Pth' $ (dropWhile (/= a) fcycle) !! (fromInteger b))
                else do
                  putStrLn $ "Directory " ++ a ++ " does not exist."
                  return Nothing
              _ -> return $
                -- Assertion.
                if val2typ (addv l r) == typ || typ == Tany
                then Just (rbs, mr, addv l r)
                else Nothing

sub :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
sub gbs typ mem lhs rhs = do
  left  <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          let
            subv x y =
              case (x, y) of
                (Int' a, Int' b)       -> Just $ Int' $ a - b
                (Int' a, Flt' b)       -> Just $ Flt' $ (fromInteger a) - b
                (Flt' a, Int' b)       -> Just $ Flt' $ a - (fromInteger b)
                (Flt' a, Flt' b)       -> Just $ Flt' $ a - b
                (Int' a, Chr' b)       -> Just $ Int' $ a - (toInteger $ C.ord b)
                (Chr' a, Int' b)       -> Just $ Chr' $ C.chr $ (C.ord a) - (fromInteger b)
                (Chr' a, Chr' b)       -> Just $ Chr' $ C.chr $ (C.ord a) - (C.ord b)
                -- (Bln' a, Bln' b)       -> Just $ Bln' $ a && b
                (Str' a, Int' b)       -> Just $ Str' $ take (fromInteger b) a
                (Arr' t a, Int' b)       -> Just $ Arr' t $ take (fromInteger b) a
                (Str' a, Chr' b)       -> Just $ Str' $ filter (/= b) a
                (Str' a, Str' b)       -> Just $ Str' $ reverse $ takeDiff (reverse a) (reverse b)
                (Arr' t1 a, Arr' t2 b) ->
                  if t1 == t2
                  then Just $ Arr' t1 (reverse (takeDiff (reverse a) (reverse b)))
                  else
                    case sequence (zipWith subv a b) of
                      Nothing -> Nothing
                      Just vals ->
                        case headMaybe vals of
                          Just front -> Just $ Arr' (val2typ front) vals
                          Nothing    -> Just $ Arr' t1 vals
                (Arr' t a, b)          ->
                  case sequence $ map (subv b) a of
                    Nothing -> Nothing
                    Just vals ->
                      case headMaybe vals of
                        Just front -> Just $ Arr' (val2typ front) vals
                        Nothing    -> Just $ Arr' t vals
                (a, Arr' t b)          ->
                  case sequence $ map (subv a) b of
                    Nothing -> Nothing
                    Just vals ->
                      case headMaybe vals of
                        Just front -> Just $ Arr' (val2typ front) vals
                        Nothing    -> Just $ Arr' t vals
                (Tup' a, Tup' b)       -> Just $ Tup' $ reverse $ takeDiff (reverse a) (reverse b)
                (Tup' a, b)            -> Just $ Tup'   $ filter (/= b) a
                (a, Tup' b)            ->
                  foldl'
                  (
                    \j i -> case j of
                    Nothing -> Nothing
                    Just j' -> subv j' i
                  )
                  (Just a) b
                _                      -> Nothing
          in
            case subv l r of
              Nothing -> return Nothing
              Just result -> return $
                -- Assertion.
                if val2typ result == typ || typ == Tany
                then Just (rbs, mr, result)
                else Nothing


mpy :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
mpy gbs typ mem lhs rhs = do
  left  <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          let
            mpyv x y =
              case (x, y) of
                (Int' a, Int' b)       -> Just $ Int' $ a * b
                (Int' a, Flt' b)       -> Just $ Flt' $ (fromInteger a) * b
                (Flt' a, Int' b)       -> Just $ Flt' $ a * (fromInteger b)
                (Flt' a, Flt' b)       -> Just $ Flt' $ a * b
                (Int' a, Chr' b)       -> Just $ Int' $ a * (toInteger $ C.ord b)
                (Chr' a, Int' b)       -> Just $ Str' $ replicate (fromInteger b) a
                (Bln' a, Bln' b)       -> Just $ Bln' $ a && b
                (Str' a, Int' b)       -> Just $ Str' $ concat $ replicate (fromInteger b) a
                -- (Arr' t a, Int' b)     -> Just $ Arr' (Tarr t) $ map (Arr' t) (replicate (fromInteger b) a)
                (Tup' a, Int' b)       -> Just $ Tup' $ map (Tup') (replicate (fromInteger b) a)
                (Arr' t1 a, Arr' t2 b) ->
                  case sequence $ map (\i -> mpyv i (Arr' t2 b)) a of
                    Just arr@(h:_) -> Just $ Arr' (val2typ h) arr 
                    Just []        -> Just $ Arr' t1 []
                    Nothing        -> Nothing
                (Arr' t a, b)          ->
                  case sequence $ map (mpyv b) a of
                    Nothing   -> Nothing
                    Just vals ->
                      case headMaybe vals of
                        Just front -> Just $ Arr' (val2typ front) vals
                        Nothing    -> Just $ Arr' t vals
                (a, Arr' t b)          ->
                  case sequence $ map (mpyv a) b of
                    Nothing   -> Nothing
                    Just vals ->
                      case headMaybe vals of
                        Just front -> Just $ Arr' (val2typ front) vals
                        Nothing    -> Just $ Arr' t vals
                (Tup' a, Tup' b)       ->
                  case sequence $ map (\i -> mpyv i (Tup' b)) a of
                    Nothing  -> Nothing
                    Just tup -> Just $ Tup' tup 
                (Tup' a, b)            ->
                  case sequence $ map (mpyv b) a of
                    Nothing  -> Nothing
                    Just tup -> Just $ Tup' tup
                (a, Tup' b)            ->
                  case sequence $ map (mpyv a) b of
                    Nothing  -> Nothing
                    Just tup -> Just $ Tup' tup
                _                      -> Nothing
          in
            case mpyv l r of
              Nothing -> return Nothing
              Just result -> return $
                -- Assertion.
                if val2typ result == typ || typ == Tany
                then Just (rbs, mr, result)
                else Nothing

-- TODO: Finish implementation
dvd :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
dvd gbs typ mem lhs rhs = do
  left  <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          let
            divList :: Int ->  [a] ->[[a]]
            divList i xs =
              let
                -- Unfair division.
                divout = length xs `div` i
                ys = replicate i divout
                lengths = zipWith (+) ys (replicate (length xs - i * divout) 1 ++ [0,0..])
                splitPlaces :: [Int] -> [a] -> [[a]]
                splitPlaces [] _ = []
                splitPlaces _ [] = []
                splitPlaces (index:is) list =
                  front: splitPlaces is back
                  where (front, back) = splitAt index list
              in
                splitPlaces lengths xs
            dvdv x y =
              case (x, y) of
                (Int' a, Int' b)       -> Just $ Int' $ a `div` b
                (Int' a, Flt' b)       -> Just $ Flt' $ (fromInteger a) / b
                (Flt' a, Int' b)       -> Just $ Flt' $ a / (fromInteger b)
                (Flt' a, Flt' b)       -> Just $ Flt' $ a / b
          --       (Bln' a, Bln' b)       -> Just $ Bln' $ a && b
                (Str' a, Int' b)       -> Just $ Arr' Tstr $ map (Str') (divList (fromInteger b) a)
                (Arr' t a, Int' b)     -> Just $ Arr' (Tarr t) $ map (Arr' t) (divList (fromInteger b) a)
                (Arr' t a, b)          ->
                  case sequence $ map (dvdv b) a of
                    Nothing   -> Nothing
                    Just vals ->
                      case headMaybe vals of
                        Just front -> Just $ Arr' (val2typ front) vals
                        Nothing    -> Just $ Arr' t vals
                (a, Arr' t b)          ->
                  case sequence $ map (dvdv a) b of
                    Nothing   -> Nothing
                    Just vals ->
                      case headMaybe vals of
                        Just front -> Just $ Arr' (val2typ front) vals
                        Nothing    -> Just $ Arr' t vals
                (Tup' a, b)            ->
                  case sequence $ map (dvdv b) a of
                    Nothing  -> Nothing
                    Just tup -> Just $ Tup' tup
                (a, Tup' b)            ->
                  case sequence $ map (dvdv a) b of
                    Nothing  -> Nothing
                    Just tup -> Just $ Tup' tup
                _                      -> Nothing
          in
            case dvdv l r of
              Nothing -> return Nothing
              Just result -> return $
                -- Assertion.
                if val2typ result == typ || typ == Tany
                then Just (rbs, mr, result)
                else Nothing

-- TODO: Finish implementation
dot :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
dot gbs typ mem lhs rhs = do
  left  <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          let
            dotv x y =
              case (x, y) of
                (Str' a, Str' b)       -> Just $ Str' $ a ++ ('.':b)
                (Tup' a, Str' b)       ->
                  case lastMaybe a of
                    Nothing -> Nothing
                    Just a' ->
                      case a' of
                        Str' s -> Just $ Tup' $ (init a) ++ [(Str' $ s ++ ('.':b))]
                        Pth' s -> Just $ Tup' $ (init a) ++ [(Pth' $ s ++ ('.':b))]
                        _      -> Nothing
                (Tup' a, Int' b)       -> a !? (fromInteger b)
                -- HIGHLY inefficient.
                (Int' a, Int' b)       -> Just $ Flt' $ read (show a ++ ('.':show b))
                (Str' a, Int' b)       ->
                  case a !? (fromInteger b) of
                    Nothing -> Nothing
                    Just c  -> Just $ Chr' c
                (Str' a, Arr' Tint b) -> Just $ Str' $ map snd (filter (\(i, _) -> Int' i `elem` b) (zip [0..] a))
                (Arr' _ a, Int' b)     -> a !? (fromInteger b)
                (Arr' t a, Arr' Tint b) -> Just $ Arr' t $ map snd (filter (\(i, _) -> Int' i `elem` b) (zip [0..] a))
                (Tup' a, Arr' Tint b) -> Just $ Tup' $ map snd (filter (\(i, _) -> Int' i `elem` b) (zip [0..] a))
                _                      -> Nothing
          in
            case dotv l r of
              Nothing -> return Nothing
              Just result -> return $
                -- Assertion.
                if val2typ result == typ || typ == Tany
                then Just (rbs, mr, result)
                else Nothing
-- Might want to case about lhs later
cd :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
cd gbs _ mem lhs rhs = do
  left  <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Just (rbs, mem', Tup' []) -> do
          home <- getHomeDirectory
          pwd <- getCurrentDirectory
          setCurrentDirectory home
          putStrLn $ "cd: " ++ pwd ++ " -> " ++ home
          return $ Just (rbs, mem', l)
        Just (rbs, mem', r) -> do
          path <- canonicalizePath $ show r
          pathExists <- doesPathExist path
          if pathExists
          then do
            pwd <- getCurrentDirectory
            putStrLn $ "cd: " ++ pwd ++ " -> " ++ path
            setCurrentDirectory $ show r
            return $ Just (rbs, mem', l)
          else
            (putStrLn $ "cd: Directory " ++ path ++ " doesn't exist.") >>=
            \_ -> return $ Nothing
        Nothing ->
          getHomeDirectory >>=
          setCurrentDirectory >>=
          \_ -> return $ Just (lbs, mem, l)

-- setenv :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
-- gbs setenv mem _ rhs = do
--   right  <- interpret gbs (Ttup $ tCollapse $ fromMaybe [Tany] $ exp2typs mem rhs) mem rhs
--   something
--   case right of
--     Just (mem', r) -> return $ Just (mem', Tup' [])

-- -- This evaluates the left side, and feeds it to the right's readProcessWithExitCode.
-- direct :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
-- _ direct = undefined

-- This is the "," operator.
next :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
next gbs typ mem lhs rhs = catch (do
  left  <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) ->
      -- Should I check for Tany?
      if l == Break || val2typ l == typ
      then return $ Just (lbs, ml, l)
      else do
        l' <- check l
        -- l' <- case lhs of
          -- Expression _ _ _ _ -> return l
          -- Operand _ -> check l
        right <-
          case exp2typ ml rhs of
            Nothing -> return Nothing
            Just t -> interpret lbs t ml rhs
        case right of
          Nothing -> return Nothing
          Just (rbs, mr, r) ->
            if val2typ r == typ
            then return $ Just (rbs, mr, r)
            else do
              r' <- check r
              -- r' <- case rhs of
                -- Expression _ _ _ _ -> return r
                -- Operand _ -> check r
              case l' of
                Tup' lt -> return $ Just (rbs, mr, vCollapse $ Tup' $ lt ++ [r'])
                _       -> return $ Just (rbs, mr, vCollapse $ Tup' [l', r'])
    ) handler
  where
    -- Should this capture the output or not?
    -- It would've been done without a thought if capturing the output
    -- didn't mean screwing up the user experience.
    check a =
        case a of
          Str' x            -> execOr x
          Tup' ((Str' x):_) -> execOr x
          Arr' Tstr (x:_)   -> execOr $ show x
          -- Str' _            -> cmd a >>= \_ -> return $ Tup' []
          -- Tup' ((Str' _):_) -> cmd a >>= \_ -> return $ Tup' []
          -- Arr' Tstr _       -> cmd a >>= \_ -> return $ Tup' []
          -- Because they aren't necessarily in PATH
          Pth' _            -> cmd a >>= \_ -> return $ Tup' []
          Tup' ((Pth' _):_) -> cmd a >>= \_ -> return $ Tup' []
          Arr' Tpth _       -> cmd a >>= \_ -> return $ Tup' []
          _ -> return a
          where
            execOr x = do
              path <- findExecutable x
              case path of
                Nothing -> return a
                Just _ ->
                  cmd a >>= \_ -> return (Tup' [])
                  -- aout <- cap a
                  -- case aout of Nothing -> return $ Tup' []
                  --   Just o  -> return $ Str' o
    handler :: IOError -> IO (Maybe a)
    handler e = do
        putStrLn $ "pipe: " ++ show e
        return Nothing

-- This takes either a tuple or a string on the left side, and feed it to the right side process.
pipe :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
pipe gbs _ mem lhs rhs = (do
  left  <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) -> do
          -- Should make it an actual pipe, but how do I do that? How do I allow chaining with that?
          -- Can't just not wait for the process. I'd have to pass it around.
          lo <-
            case l of
              Tup' (Str' x:_) -> do
                x' <- findExecutable x
                case x' of
                  Nothing -> return $ Just $ show l
                  Just _ -> cap l
              -- Tup' (Str' _:_) -> cap l
              -- Not sure about this.
              -- Str' _          -> cap l
              Str' x          -> do
                x' <- findExecutable x
                case x' of
                  Nothing -> return $ Just $ show l
                  Just _ -> cap l
              Tup' (Pth' _:_) -> cap l
              Pth' _          -> cap l
              -- _               -> return Nothing
              _               -> return $ Just $ show l
          let
            lout = fromMaybe "" lo
            exec x xs = do
              x' <- findExecutable x
              case x' of
                Nothing -> return $ Just (rbs, mr, Str' $ lout ++ ' ':(show $ vCollapse $ Tup' $ Str' x:xs))
                Just _  -> do
                  rarg <- argIO xs
                  (inp, out, _, randle) <- createProcess (proc x rarg) {std_in = CreatePipe, std_out = CreatePipe}
                  case (inp, out) of
                    (Just i, Just o) -> do
                      hSetBinaryMode o True
                      txt <- hGetContents o
                      hPutStr i lout
                      hClose i
                      -- putStrLn txt
                      exitcode <- waitForProcess randle
                      if exitcode /= ExitSuccess
                      then putStrLn $ "Process " ++ x ++ (unwords rarg) ++ " exited with exit code: " ++ show exitcode
                      else return ()
                      return $ Just (rbs, mr, Str' txt)
                    _ -> do
                      putStrLn $ "Could not create process " ++ x ++ (unwords rarg) ++ " properly."
                      return Nothing
          -- let lout = show l
          case r of
            Tup' (Str' x:xs) -> exec x xs
            Tup' (Pth' x:xs) -> exec x xs
            _ -> exec (show r) []) `catch` handler
  where
    handler :: IOError -> IO (Maybe a)
    handler e = do
        putStrLn $ "pipe: " ++ show e
        return Nothing

len :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
len gbs _ mem lhs rhs = do
  left  <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          case r of
            Int'   b -> return $ Just (rbs, mr, Tup' [l, Int' $ toInteger $ length $ show b])
            Flt'   b -> return $ Just (rbs, mr, Tup' [l, Int' $ toInteger $ length $ show b])
            Arr' _ b -> return $ Just (rbs, mr, Tup' [l, Int' $ toInteger $ length b])
            Tup'   b -> return $ Just (rbs, mr, Tup' [l, Int' $ toInteger $ length b])
            Pth'   b -> return $ Just (rbs, mr, Tup' [l, Int' $ toInteger $ length $ pieces (== '/') b])
            _        -> return Nothing

range :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
range gbs _ mem lhs rhs = do
  left  <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          case (l, r) of
            (Int' a, Int' b) -> return $ Just (rbs, mr, Arr' Tint $ map (Int') [a..b])
            (Tup' [Int' a, Int' d], Int' b) -> return $ Just (rbs, mr, Arr' Tint $ map (Int') [a,d..b])
            -- This is like, really bad for accuracy but whatever...
            (Tup' [Flt' a, Int' d], Int' b) -> return $ Just (rbs, mr, Arr' Tflt $ map (Flt' . float2Double) [double2Float a,fromInteger d.. fromInteger b])
            (Tup' [Int' a, Flt' d], Int' b) -> return $ Just (rbs, mr, Arr' Tflt $ map (Flt' . float2Double) [fromInteger a,double2Float d.. fromInteger b])
            (Tup' [Flt' a, Flt' d], Int' b) -> return $ Just (rbs, mr, Arr' Tflt $ map (Flt' . float2Double) [double2Float a,double2Float d.. fromInteger b])
            (Tup' [Int' a, Int' d], Flt' b) -> return $ Just (rbs, mr, Arr' Tflt $ map (Flt' . float2Double) [fromInteger a,fromInteger d.. double2Float b])
            (Tup' [Flt' a, Int' d], Flt' b) -> return $ Just (rbs, mr, Arr' Tflt $ map (Flt' . float2Double) [double2Float a,fromInteger d.. double2Float b])
            (Tup' [Int' a, Flt' d], Flt' b) -> return $ Just (rbs, mr, Arr' Tflt $ map (Flt' . float2Double) [fromInteger a,double2Float d.. double2Float b])
            (Tup' [Flt' a, Flt' d], Flt' b) -> return $ Just (rbs, mr, Arr' Tflt $ map (Flt' . float2Double) [double2Float a,double2Float d.. double2Float b])
            (Chr' a, Chr' b) -> return $ Just (rbs, mr, Arr' Tchr $ map (Chr') [a..b])
            (Tup' [Chr' a, Chr' d], Chr' b) -> return $ Just (rbs, mr, Arr' Tchr $ map (Chr') [a,d..b])
            (Pth' a, Pth' b) ->
              let
                parentA = reverse (dropWhile (/= '/') (reverse a))
                parentB = reverse (dropWhile (/= '/') (reverse b))
              in
                if parentA == parentB
                then do
                  exists <- doesPathExist parentA
                  if exists
                  then do
                    files <- listDirectory ('/':parentA)
                    let fcycle = cycle files
                    return $ Just (rbs, mr, Arr' Tpth $ map (Pth') (takeWhile (/= b) (dropWhile (/= a) fcycle)))
                  else do
                    putStrLn $ "Directory " ++ a ++ " does not exist."
                    return Nothing
                else return Nothing                  
            _                -> return Nothing

input :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
input gbs _ mem lhs rhs = do
  left <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) -> do
          line <- getLine
          case (l, r) of
            (Tup' [], Tup' []) -> return $ Just (rbs, mr, Str' line)
            _ -> return Nothing

equal :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
equal gbs _ mem lhs rhs = do
  left <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          return $ Just (rbs, mr, Bln' (l == r))

unequal :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
unequal gbs _ mem lhs rhs = do
  left <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          return $ Just (rbs, mr, Bln' (l /= r))

less :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
less gbs _ mem lhs rhs = do
  left <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          return $ Just (rbs, mr, Bln' (l < r))

loe :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
loe gbs _ mem lhs rhs = do
  left <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          return $ Just (rbs, mr, Bln' (l <= r))

more :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
more gbs _ mem lhs rhs = do
  left <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          return $ Just (rbs, mr, Bln' (l > r))

moe :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
moe gbs _ mem lhs rhs = do
  left <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          return $ Just (rbs, mr, Bln' (l >= r))

opposite :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
opposite gbs _ mem lhs rhs = do
  left <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          case (l, r) of
            (Tup' [], Bln' b) -> return $ Just (rbs, mr, Bln' $ not b)
            _                 -> return Nothing

and :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
and gbs _ mem lhs rhs = do
  left <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    -- This is because If the right side depends on the left side and becomes invalid, that'd be bad.
    Just (_, _, Bln' True) -> return left
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          case (l, r) of
            (Bln' a, Bln' b) -> return $ Just (rbs, mr, Bln' $ a || b)
            _                 -> return Nothing

or :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
or gbs _ mem lhs rhs = do
  left <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    -- This is because If the right side depends on the left side and becomes invalid, that'd be bad.
    Just (_, _, Bln' True) -> return left
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          case (l, r) of
            (Bln' a, Bln' b) -> return $ Just (rbs, mr, Bln' $ a || b)
            _                 -> return Nothing

int :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
int gbs _ mem lhs rhs = do
  left <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          case (l, r) of
            (Tup' [], Int' _) -> return $ Just (rbs, mr, r)
            (Tup' [], Flt' b) -> return $ Just (rbs, mr, Int' $ floorDouble b)
            (Tup' [], Bln' b) ->
              return $ Just
              (
                rbs, mr, Int' $
                  case b of
                    True -> 1
                    False -> 2
              )
            (Tup' [], Chr' b) -> return $ Just (rbs, mr, Int' $ (toInteger . C.ord) b)
            (Tup' [], Str' b) ->
              case intMaybe b of
                Nothing -> return Nothing
                Just i -> return $ Just (rbs, mr, Int' i)
            _                 -> return Nothing

flt :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
flt gbs _ mem lhs rhs = do
  left <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          case (l, r) of
            (Tup' [], Int' b) -> return $ Just (rbs, mr, Flt' $ fromInteger b)
            (Tup' [], Flt' _) -> return $ Just (rbs, mr, r)
            (Tup' [], Bln' b) ->
              return $ Just
              (
                rbs, mr, Flt' $
                  case b of
                    True -> 1.0
                    False -> 2.0
              )
            (Tup' [], Chr' b) -> return $ Just (rbs, mr, Flt' $ (fromIntegral . C.ord) b)
            (Tup' [], Str' b) ->
              case readMaybe b of
                Nothing -> return Nothing
                Just f -> return $ Just (rbs, mr, Flt' f)
            _                 -> return Nothing

bln :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
bln gbs _ mem lhs rhs = do
  left <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          case (l, r) of
            (Tup' [], Int' b) ->
              case b of
                0 -> return $ Just (rbs, mr, Bln' False)
                1 -> return $ Just (rbs, mr, Bln' True)
                _ -> return Nothing
            (Tup' [], Flt' b) ->
              case b of
                0 -> return $ Just (rbs, mr, Bln' False)
                1 -> return $ Just (rbs, mr, Bln' True)
                _ -> return Nothing
            (Tup' [], Bln' _) -> return $ Just (rbs, mr, r)
            (Tup' [], Chr' b) ->
              case b of
                't' -> return $ Just (rbs, mr, Bln' True)
                'T' -> return $ Just (rbs, mr, Bln' True)
                'y' -> return $ Just (rbs, mr, Bln' True)
                'Y' -> return $ Just (rbs, mr, Bln' True)
                'f' -> return $ Just (rbs, mr, Bln' False)
                'F' -> return $ Just (rbs, mr, Bln' False)
                'n' -> return $ Just (rbs, mr, Bln' False)
                'N' -> return $ Just (rbs, mr, Bln' False)
                _   -> return Nothing
            (Tup' [], Str' b) ->
              case readMaybe b of
                Nothing -> return Nothing
                Just f -> return $ Just (rbs, mr, Bln' f)
            _                 -> return Nothing

chr :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
chr gbs _ mem lhs rhs = do
  left <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          case (l, r) of
            (Tup' [], Int' b) -> return $ Just (rbs, mr, Chr' $ (C.chr . fromIntegral) b)
            (Tup' [], Flt' b) -> return $ Just (rbs, mr, Chr' $ (C.chr . double2Int) b)
            (Tup' [], Bln' b) ->
              return $ Just
              (
                rbs, mr, Chr' $
                  case b of
                    True -> 'T'
                    False -> 'F'
              )
            (Tup' [], Chr' _) -> return $ Just (rbs, mr, r)
            (Tup' [], Str' [b]) -> return $ Just (rbs, mr, Chr' b)
            _                 -> return Nothing

str :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
str gbs _ mem lhs rhs = do
  left <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          case l of
            Tup' [] -> return $ Just (rbs, mr, Str' $ show r)
            _       -> return Nothing

pth :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
pth gbs _ mem lhs rhs = do
  left <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          case (l, r) of
            (Tup' [], Str' b) -> return $ Just (rbs, mr, Pth' b)
            (Tup' [], Pth' _) -> return $ Just (rbs, mr, r)
            (Tup' [], Arr' Tstr b) -> return $ Just (rbs, mr, Pth' (intercalate "/" $ map (show) b))
            _       -> return Nothing

ift :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
ift gbs typ mem lhs rhs =
  case lhs of
    Operand (Tup []) ->
      case rhs of
        Operand (Tup [predicate, expression]) -> do
          left <- ((interpret gbs Tbln mem) . parse) [predicate]
          case left of
            Nothing -> return Nothing
            Just (_, ml, l) ->
              if l == Bln' True
              then ((interpret True typ ml) . parse) [expression]
              else return $ Just (False, ml, Tup' [])
        _                          -> return Nothing
    _                -> do
      right <-
        case exp2typ mem rhs of
          Just Tbln -> interpret gbs Tbln mem rhs
          Just Tany -> interpret gbs Tbln mem rhs
          _         -> return Nothing
      case right of
        Nothing -> return Nothing
        Just (_, mr, Bln' True)  ->
            case exp2typ mr lhs of
              Nothing -> return Nothing
              Just t  -> interpret True t mr lhs
        Just (_, mr, Bln' False) -> return $ Just (False, mr, Tup' [])
        _                        -> return Nothing

els :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
els gbs typ mem lhs rhs = do
  left <-
    case exp2typ mem lhs of
      Just Tbln -> interpret gbs Tbln mem lhs
      Just Tany -> interpret gbs Tbln mem lhs
      _         -> return Nothing
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, _) ->
      if lbs == False
      then interpret lbs typ ml rhs
      else return $ Just (lbs, ml, Tup' [])

-- Make it just if but it's recursive until the predicate evaluates to false.
whl :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
whl gbs typ mem lhs rhs =
  case lhs of
    Operand (Tup []) ->
      case rhs of
        Operand (Tup [predicate, expression]) -> do
          left <- ((interpret gbs Tbln mem) . parse) [predicate]
          case left of
            Just (_, ml, Bln' True) -> do
                iteration <- ((interpret True typ ml) . parse) [expression]
                case iteration of
                  Nothing -> return Nothing
                  Just (_, mr, Break) -> return $ Just (True, mr, Tup' [])
                  Just (_, mr, r) -> do
                    final <- whl True typ mr lhs rhs
                    case final of
                      Nothing -> return Nothing
                      Just (fbs, mf, Tup' f) -> return $ Just (fbs, mf, Tup' (r:f))
                      Just (fbs, mf,      f) -> return $ Just (fbs, mf, Tup' [r, f])
            Just (_, ml, Bln' False) -> return $ Just (False, ml, Tup' [])
            _                        -> return Nothing
        _ -> return Nothing
    _ -> do
      right <-
        case exp2typ mem rhs of
          Just Tbln -> interpret gbs Tbln mem rhs
          Just Tany -> interpret gbs Tbln mem rhs
          _         -> return Nothing
      case right of
        Just (_, mr, Bln' True)  -> do
            case exp2typ mr rhs of
              Nothing -> return Nothing
              Just _  -> do
                iteration <- interpret True typ mem lhs
                case iteration of
                  Nothing -> return Nothing
                  Just (_, ml, Break) -> return $ Just (True, ml, Tup' [])
                  Just (_, ml, l) -> do
                    final <- whl True typ ml lhs rhs
                    case final of
                      Nothing -> return Nothing
                      Just (fbs, mf, Tup' f) -> return $ Just (fbs, mf, Tup' (l:f))
                      Just (fbs, mf,      f) -> return $ Just (fbs, mf, Tup' [l, f])
        Just (_, mr, Bln' False) -> return $ Just (False, mr, Tup' [])
        _ -> return Nothing

-- Deals with for (initialize, predicate, increment) loops.
frl :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
frl gbs typ mem lhs rhs = undefined

-- Deals with foreach (variable, list of values) loops.
fre :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
fre gbs typ mem lhs rhs = undefined
