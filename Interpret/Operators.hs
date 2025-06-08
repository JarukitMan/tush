module Interpret.Operators where

import Misc
import Interpret.Data
import Interpret.Evaluate
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

add :: Type -> Memory -> Expression -> Expression -> IO (Maybe (Memory, Value))
add typ mem lhs rhs = do
  left  <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret t mem lhs
  case left of
    Nothing -> return Nothing
    Just (ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret t ml rhs
      case right of
        Nothing -> return Nothing
        Just (mr, r) ->
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
                  files <- getDirectoryContents dir
                  let fcycle = cycle files
                  return $ Just (mr, Pth' $ (dropWhile (/= a) fcycle) !! (fromInteger b))
                else do
                  putStrLn $ "Directory " ++ a ++ " does not exist."
                  return Nothing
              _ -> return $
                -- Assertion.
                if val2typ (addv l r) == typ || typ == Tany
                then Just (mr, addv l r)
                else Nothing

sub :: Type -> Memory -> Expression -> Expression -> IO (Maybe (Memory, Value))
sub typ mem lhs rhs = do
  left  <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret t mem lhs
  case left of
    Nothing -> return Nothing
    Just (ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret t ml rhs
      case right of
        Nothing -> return Nothing
        Just (mr, r) ->
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
                then Just (mr, result)
                else Nothing


mpy :: Type -> Memory -> Expression -> Expression -> IO (Maybe (Memory, Value))
mpy typ mem lhs rhs = do
  left  <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret t mem lhs
  case left of
    Nothing -> return Nothing
    Just (ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret t ml rhs
      case right of
        Nothing -> return Nothing
        Just (mr, r) ->
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
                then Just (mr, result)
                else Nothing

-- TODO: Finish implementation
dvd :: Type -> Memory -> Expression -> Expression -> IO (Maybe (Memory, Value))
dvd typ mem lhs rhs = do
  left  <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret t mem lhs
  case left of
    Nothing -> return Nothing
    Just (ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret t ml rhs
      case right of
        Nothing -> return Nothing
        Just (mr, r) ->
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
                then Just (mr, result)
                else Nothing

-- TODO: Finish implementation
dot :: Type -> Memory -> Expression -> Expression -> IO (Maybe (Memory, Value))
dot typ mem lhs rhs = do
  left  <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret t mem lhs
  case left of
    Nothing -> return Nothing
    Just (ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret t ml rhs
      case right of
        Nothing -> return Nothing
        Just (mr, r) ->
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
                then Just (mr, result)
                else Nothing
-- Might want to case about lhs later
cd :: Type -> Memory -> Expression -> Expression -> IO (Maybe (Memory, Value))
cd _ mem lhs rhs = do
  left  <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret t mem lhs
  case left of
    Nothing -> return Nothing
    Just (ml, _) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret t ml rhs
      case right of
        Just (mem', Tup' []) -> do
          home <- getHomeDirectory
          pwd <- getCurrentDirectory
          setCurrentDirectory home
          putStrLn $ "cd: " ++ pwd ++ " -> " ++ home
          return $ Just (mem', Tup' [])
        Just (mem', r) -> do
          pth <- canonicalizePath $ show r
          pathExists <- doesPathExist pth
          if pathExists
          then do
            pwd <- getCurrentDirectory
            putStrLn $ "cd: " ++ pwd ++ " -> " ++ pth
            setCurrentDirectory $ show r
          else putStrLn $ "cd: Directory " ++ pth ++ " doesn't exist."
          return $ Just (mem', Tup' [])
        Nothing ->
          getHomeDirectory >>=
          setCurrentDirectory >>=
          \_ -> return $ Just (mem, Tup' [])

-- setenv :: Type -> Memory -> Expression -> Expression -> IO (Maybe (Memory, Value))
-- setenv mem _ rhs = do
--   right  <- interpret (Ttup $ tCollapse $ fromMaybe [Tany] $ exp2typs mem rhs) mem rhs
--   something
--   case right of
--     Just (mem', r) -> return $ Just (mem', Tup' [])

-- -- This evaluates the left side, and feeds it to the right's readProcessWithExitCode.
-- direct :: Type -> Memory -> Expression -> Expression -> IO (Maybe (Memory, Value))
-- direct = undefined

-- This is the "," operator.
next :: Type -> Memory -> Expression -> Expression -> IO (Maybe (Memory, Value))
next typ mem lhs rhs = catch (do
  left  <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret t mem lhs
  case left of
    Nothing -> return Nothing
    Just (ml, l) ->
      -- Should I check for Tany?
      if val2typ l == typ
      then return $ Just (ml, l)
      else do
        l' <- check l
        -- l' <- case lhs of
          -- Expression _ _ _ _ -> return l
          -- Operand _ -> check l
        right <-
          case exp2typ ml rhs of
            Nothing -> return Nothing
            Just t -> interpret t ml rhs
        case right of
          Nothing -> return Nothing
          Just (mr, r) ->
            if val2typ r == typ
            then return $ Just (mr, r)
            else do
              r' <- check r
              -- r' <- case rhs of
                -- Expression _ _ _ _ -> return r
                -- Operand _ -> check r
              case l' of
                Tup' lt -> return $ Just (mr, vCollapse $ Tup' $ lt ++ [r'])
                _       -> return $ Just (mr, vCollapse $ Tup' [l', r'])
    ) handler
  where
    -- Should this capture the output or not?
    -- It would've been done without a thought if capturing the output
    -- didn't mean screwing up the user experience.
    check a =
        case a of
          Arr' Tstr (x:_)   -> execOr $ show x
          Str' x            -> execOr x
          Tup' ((Str' x):_) -> execOr x
          -- Because there aren't necessarily in PATH
          Pth' _            -> cmd a >>= \_ -> return $ Tup' []
          Tup' ((Pth' _):_) -> cmd a >>= \_ -> return $ Tup' []
          _ -> return a
          where
            execOr x = do
              pth <- findExecutable x
              case pth of
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
pipe :: Type -> Memory -> Expression -> Expression -> IO (Maybe (Memory, Value))
pipe _ mem lhs rhs = (do
  left  <-
    case exp2typ mem lhs of
      Nothing -> return Nothing
      Just t -> interpret t mem lhs
  case left of
    Nothing -> return Nothing
    Just (ml, l) -> do
      right <-
        case exp2typ ml rhs of
          Nothing -> return Nothing
          Just t -> interpret t ml rhs
      case right of
        Nothing -> return Nothing
        Just (mr, r) -> do  
          -- Should make it an actual pipe, but how do I do that? How do I allow chaining with that?
          -- Can't just not wait for the process. I'd have to pass it around.
          lo <-
            case l of
              Tup' (Str' x:_) -> do
                x' <- findExecutable x
                case x' of
                  Nothing -> return $ Just $ show l
                  Just _ -> cap l
              -- Not sure about this.
              Str' x          -> do
                x' <- findExecutable x
                case x' of
                  Nothing -> return $ Just $ show l
                  Just _ -> cap l
              Tup' (Pth' _:_) -> cap l
              Pth' _          -> cap l
              _               -> return $ Just $ show l
          let
            lout = fromMaybe "" lo
            exec x xs = do
              x' <- findExecutable x
              case x' of
                Nothing -> return $ Just (mr, Str' $ lout ++ ' ':(show $ vCollapse $ Tup' $ Str' x:xs))
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
                      return $ Just (mr, Str' txt)
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

