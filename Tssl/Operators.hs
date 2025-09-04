{-# LANGUAGE OverloadedStrings #-}
module Tssl.Operators where

import Misc
import Tssl.Data
import Tssl.Evaluate
import Tssl.Parse
import Data.Maybe
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Char as C
import System.Directory
import System.Process
import System.IO
import System.Exit
import System.Environment
import Control.Exception
import GHC.Float

add :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
add gbs etp mem lhs rhs = do
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
                (Pth' a, Str' b)       -> Pth' $ a ++ ('/':T.unpack b)
                (Str' a, Chr' b)       -> Str' $ a `T.snoc` b
                (Chr' a, Str' b)       -> Str' $ a `T.cons` b
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
                  cdir <- canonicalizePath dir
                  fs <- listDirectory cdir
                  let
                    files = map ((cdir ++) . ('/':)) fs
                    fcycle = cycle files
                  ca <- canonicalizePath a
                  return $ maybe Nothing (\x -> Just (rbs, mr, Pth' $ x)) $ (dropWhile (/= ca) fcycle) !? (fromInteger b)
                else do
                  putStrLn $ "Directory " ++ a ++ " does not exist."
                  return Nothing
              _ -> return $
                -- Assertion.
                if val2typ (addv l r) == etp || etp == Tany
                then Just (rbs, mr, addv l r)
                else Nothing

sub :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
sub gbs etp mem lhs rhs = do
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
                (Tup' [], Int' b)      -> Just $ Int' $ -b
                (Tup' [], Flt' b)      -> Just $ Flt' $ -b
                (Int' a, Int' b)       -> Just $ Int' $ a - b
                (Int' a, Flt' b)       -> Just $ Flt' $ (fromInteger a) - b
                (Flt' a, Int' b)       -> Just $ Flt' $ a - (fromInteger b)
                (Flt' a, Flt' b)       -> Just $ Flt' $ a - b
                (Int' a, Chr' b)       -> Just $ Int' $ a - (toInteger $ C.ord b)
                (Chr' a, Int' b)       -> Just $ Chr' $ C.chr $ (C.ord a) - (fromInteger b)
                (Chr' a, Chr' b)       -> Just $ Chr' $ C.chr $ (C.ord a) - (C.ord b)
                (Str' a, Int' b)       -> Just $ Str' $ T.take (T.length a - fromInteger b) a
                (Arr' t a, Int' b)       -> Just $ Arr' t $ take (length a - fromInteger b) a
                (Str' a, Chr' b)       -> Just $ Str' $ T.filter (/= b) a
                (Str' a, Str' b)       -> Just $ Str' $ T.pack $ reverse $ takeDiff (reverse $ T.unpack a) (reverse $ T.unpack b)
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
            case (l, r) of
              (Pth' a, Int' b) -> do
                let dir = a ++ "/.."
                exists <- doesPathExist dir
                if exists
                then do
                  cdir <- canonicalizePath dir
                  fs <- listDirectory cdir
                  let
                    files = map ((cdir ++) . ('/':)) fs
                    fcycle = cycle files
                  ca <- canonicalizePath a
                  return $ maybe Nothing (\x -> Just (rbs, mr, Pth' $ x)) $ (dropWhile (/= ca) fcycle) !? (fromInteger b)
                else do
                  T.putStrLn $ "Directory " `T.append` (T.pack a) `T.append` " does not exist."
                  return Nothing
              _ ->
                case subv l r of
                  Nothing -> return Nothing
                  Just result -> return $
                    -- Assertion.
                    if val2typ result == etp || etp == Tany
                    then Just (rbs, mr, result)
                    else Nothing


mpy :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
mpy gbs etp mem lhs rhs = do
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
                (Chr' a, Int' b)       -> Just $ Str' $ T.pack $ replicate (fromInteger b) a
                (Bln' a, Bln' b)       -> Just $ Bln' $ a && b
                (Str' a, Int' b)       -> Just $ Str' $ T.concat $ replicate (fromInteger b) a
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
                if val2typ result == etp || etp == Tany
                then Just (rbs, mr, result)
                else Nothing

-- TODO: Finish implementation
dvd :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
dvd gbs etp mem lhs rhs = do
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
                (Str' a, Int' b)       -> Just $ Arr' Tstr $ map (Str' . T.pack) (divList (fromInteger b) (T.unpack a))
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
                if val2typ result == etp || etp == Tany
                then Just (rbs, mr, result)
                else Nothing

-- TODO: Finish implementation
acs :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
acs gbs etp mem lhs rhs = do
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
                (Tup' a, Int' b)       -> a !? (fromInteger b)
                (Str' a, Int' b)       ->
                  case T.uncons $ T.drop (fromInteger b) a of
                    Nothing -> Nothing
                    Just (c, _)  -> Just $ Chr' c
                (Str' a, Arr' Tint b) -> Just $ Str' $ T.pack $ map snd (filter (\(i, _) -> Int' i `elem` b) (zip [0..] (T.unpack a)))
                (Arr' _ a, Int' b)     -> a !? (fromInteger b)
                (Arr' t a, Arr' Tint b) -> Just $ Arr' t $ map snd (filter (\(i, _) -> Int' i `elem` b) (zip [0..] a))
                (Tup' a, Arr' Tint b) -> Just $ Tup' $ map snd (filter (\(i, _) -> Int' i `elem` b) (zip [0..] a))
                _                      -> Nothing
          in
            case dotv l r of
              Nothing ->
                case (l, r) of
                  (Pth' dir, Int' i) -> do
                    exists <- doesPathExist dir
                    if exists
                    then do
                      cdir <- canonicalizePath dir
                      fs <- listDirectory cdir
                      let
                        files = map ((cdir ++) . ('/':)) fs
                        fcycle = cycle files
                      return $ maybe Nothing (\x -> Just (rbs, mr, Pth' $ x)) $ fcycle !? (fromInteger i)
                      -- return $ Just (rbs, mr, Pth' $ fcycle !! (fromInteger i))
                    else
                      return Nothing
                  _ -> return Nothing
              Just result -> return $
                -- Assertion.
                if val2typ result == etp || etp == Tany
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
          T.putStrLn $ "cd: " `T.append` (T.pack pwd) `T.append` " -> " `T.append` (T.pack home)
          return $ Just (rbs, mem', l)
        Just (rbs, mem', r) -> do
          path <- canonicalizePath $ T.unpack $ T.intercalate " " $ argify r
          pathExists <- doesPathExist path
          if pathExists
          then do
            pwd <- getCurrentDirectory
            T.putStrLn $ "cd: " `T.append` (T.pack pwd) `T.append` " -> " `T.append` (T.pack path)
            setCurrentDirectory path
            return $ Just (rbs, mem', l)
          else
            (T.putStrLn $ "cd: Directory " `T.append` (T.pack path) `T.append` " doesn't exist.") >>=
            \_ -> return $ Nothing
        Nothing -> return Nothing
          -- getHomeDirectory >>=
          -- setCurrentDirectory >>=
          -- \_ -> return $ Just (lbs, mem, l)

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
next gbs etp mem lhs rhs = catch (do
  left  <-
    interpret gbs etp mem lhs
    -- case exp2typ mem lhs of
    --   Nothing -> return Nothing
    --   Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) ->
      -- Should I check for Tany?
      if l == Break || val2typ l == etp
      then return $ Just (lbs, ml, l)
      else do
        l' <- check l
        -- l' <- case lhs of
          -- Expression _ _ _ _ -> return l
          -- Operand _ -> check l
        -- right <- interpret lbs etp ml rhs
        right <- interpret lbs etp ml (parse ml (flatten rhs))
          -- case exp2typ ml rhs of
          --   Nothing -> return Nothing
          --   Just t -> interpret lbs t ml rhs
        case right of
          Nothing -> return Nothing
          Just (rbs, mr, r) ->
            if val2typ r == etp
            then return $ Just (rbs, mr, r)
            else do
              r' <- check r
              -- r' <- case rhs of
                -- Expression _ _ _ _ -> return r
                -- Operand _ -> check r
              case l' of
                Tup' [] -> return $ Just (rbs, mr, vCollapse $ Tup' [Tup' [], r'])
                Tup' lt -> return $ Just (rbs, mr, vCollapse $ Tup' $ lt ++ [r'])
                _       -> return $ Just (rbs, mr, vCollapse $ Tup' [l', r'])
    ) handler
  where
    -- Should this capture the output or not?
    -- It would've been done without a thought if capturing the output
    -- didn't mean screwing up the user experience.
    exec :: String -> [Value] -> IO Value
    exec x xs =
      createProcess (proc x (map T.unpack (argify $ Tup' xs))) >>=
      -- argIO xs >>=
      -- \args -> createProcess (proc x (map T.unpack args)) >>=
      \(_, _, _, h) -> waitForProcess h >>=
      \_ -> return $ Tup' []
    check a =
        case vFlatten a of
          [Str' x]            -> execOr (T.unpack x) []
          -- Tup' ((Str' x):_) -> execOr x
          -- Arr' Tstr (x:_)   -> execOr $ show x
          -- Str' _            -> cmdWait a
          ((Str' x):xs) -> exec (T.unpack x) xs
          -- Arr' Tstr (x:xs)       -> exec (show x) xs
          -- Because they aren't necessarily in PATH
          [Pth' x]            -> execOr x []
          ((Pth' x):xs) -> execOr x xs
          -- Arr' Tpth (x:xs)       -> exec (show x) xs
          [Out' o _]          -> do
            -- FIXME: Worst code I have ever written to date.
            (_, _, _, process) <- createProcess (proc "cat" []) {std_in = UseHandle o}
            exitCode <- waitForProcess process
            -- Screw this, I'm passing it to cat.
            -- output <- do
            --   -- hSetBuffering out NoBuffering
            --   hSetBinaryMode o True
            --   B.hGetContents o
            -- T.putStr (T.decodeUtf8Lenient output)
            -- exitCode <- waitForProcess h
            -- hDuplicateTo tmpout stdout
            if exitCode /= ExitSuccess
            then T.putStrLn $ "Process " `T.append` T.show a `T.append` " exited with exit code: " `T.append` T.show exitCode
            else return ()
            -- Bash does this too. Might as well, since I don't want my args to contain newlines.
            return $ Tup' []
          _ -> return a
          where
            execOr x xs = do
              path <- findExecutable x
              case path of
                Nothing -> return a
                Just _ -> exec (show a) xs
                  -- aout <- cap a
                  -- case aout of Nothing -> return $ Tup' []
                  --   Just o  -> return $ Str' o
    handler :: IOError -> IO (Maybe a)
    handler e = do
        T.putStrLn $ "next: " `T.append` T.show e
        return Nothing

-- The "&" operator
-- In bash, jobs are sent to the background. We'll have to implement that later. Soon.
also :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
also gbs etp mem lhs rhs = catch (do
  left  <-
    interpret gbs etp mem lhs
    -- case exp2typ mem lhs of
    --   Nothing -> return Nothing
    --   Just t -> interpret gbs t mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) ->
      -- Should I check for Tany?
      if l == Break || val2typ l == etp
      then return $ Just (lbs, ml, l)
      else do
        l' <- check l
        -- l' <- case lhs of
          -- Expression _ _ _ _ -> return l
          -- Operand _ -> check l
        right <- interpret lbs etp ml rhs
          -- case exp2typ ml rhs of
          --   Nothing -> return Nothing
          --   Just t -> interpret lbs t ml rhs
        case right of
          Nothing -> return Nothing
          Just (rbs, mr, r) ->
            if val2typ r == etp
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
        case vFlatten a of
          (Str' _:_) -> cmd a >>= \_ -> return $ Tup' []
          (Pth' _:_) -> cmd a >>= \_ -> return $ Tup' []
          (Out' _ _:_) -> return $ Tup' []
          -- Str' _            -> cmd a >>= \_ -> return $ Tup' []
          -- Tup' ((Str' x):_) -> execOr x
          -- Arr' Tstr (x:_)   -> execOr $ show x
          -- Str' _            -> cmdConc a >>= \_ -> return $ Tup' []
          -- Tup' ((Str' _):_) -> cmd a >>= \_ -> return $ Tup' []
          -- Arr' Tstr _       -> cmd a >>= \_ -> return $ Tup' []
          -- Because they aren't necessarily in PATH
          -- Pth' _            -> cmd a >>= \_ -> return $ Tup' []
          -- Tup' ((Pth' _):_) -> cmd a >>= \_ -> return $ Tup' []
          -- Arr' Tpth _       -> cmd a >>= \_ -> return $ Tup' []
          _ -> return a
          -- where
          --   execOr x = do
          --     path <- findExecutable x
          --     case path of
          --       Nothing -> return a
          --       Just _ ->
          --         cmdConc a >>= \_ -> return (Tup' [])
          --         -- aout <- cap a
          --         -- case aout of Nothing -> return $ Tup' []
          --         --   Just o  -> return $ Str' o
    handler :: IOError -> IO (Maybe a)
    handler e = do
        T.putStrLn $ "also: " `T.append` T.show e
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
          lo <-
            case vFlatten l of
              (Str' _:_)          -> cmd l
              (Pth' _:_)          -> cmd l
              -- Tup' (Str' x:_) -> do
                -- x' <- findExecutable x
                -- case x' of
                --   Nothing -> return $ Just $ show l
                --   Just _ -> cap l
              -- Tup' (Str' _:_) -> cmd l
              -- Not sure about this.
              -- Str' _          -> cmd l
              -- Str' _          -> cap l
              -- Str' x          -> do
              --   x' <- findExecutable x
              --   case x' of
              --     Nothing -> return $ Just $ show l
              --     Just _ -> cap l
              -- Tup' (Pth' _:_) -> cmd l
              -- Pth' _          -> cmd l
              [Out' o h]        -> return $ Just (o, h)
              _               -> return Nothing
              -- _               -> return $ Just $ show l
          let
            -- lout = fromMaybe "" lo
            exec :: String -> [Value] -> IO (Maybe (Bool, Memory, Value))
            exec x xs = do
              x' <- findExecutable x
              case x' of
                Nothing -> return Nothing
                -- Nothing -> return $ Just (rbs, mr, Str' $ lout ++ ' ':(show $ vCollapse $ Tup' $ Str' x:xs))
                Just x''  -> do
                  let rarg = argify (Tup' xs)
                  -- rarg <- argIO xs
                  case lo of
                    Nothing ->
                      T.putStrLn ("Could not create process " `T.append` (T.pack x) `T.append` (T.unwords rarg) `T.append` " properly.") >>=
                      \_ -> return Nothing
                    Just (i, _) -> do
                      (_, out, _, randle) <- createProcess (proc x'' (map T.unpack rarg)) {std_in = UseHandle i, std_out = CreatePipe}
                      case out of
                        Nothing -> return Nothing
                        Just o  -> return $ Just (rbs, mr, Out' o randle)
                  -- (inp, out, _, randle) <- createProcess (proc x'' (map T.unpack rarg)) {std_in = CreatePipe, std_out = CreatePipe}
                  -- case (inp, out) of
                  --   (Just i, Just o) -> do
                  --     txt <- do
                  --       T.hPutStr i lout
                  --       hSetBinaryMode o True
                  --       hClose i
                  --       B.hGetContents o
                  --     -- putStrLn txt
                  --     exitcode <- waitForProcess randle
                  --     if exitcode /= ExitSuccess
                  --     then T.putStrLn $ "Process " `T.append` x `T.append` (T.unwords rarg) `T.append` " exited with exit code: " `T.append` T.show exitcode
                  --     else return ()
                  --     return $ Just (rbs, mr, Str' $ T.decodeUtf8Lenient txt)
                  --   _ -> do
                  --     T.putStrLn $ "Could not create process " `T.append` x `T.append` (T.unwords rarg) `T.append` " properly."
                  --     return Nothing
          -- let lout = show l
          case vFlatten r of
            (Str' x:xs) -> exec (T.unpack x) xs
            (Pth' x:xs) -> exec x xs
            -- Sketchy.
            [Out' _ _ ] -> return $ Just (rbs, mr, r)
            _ -> exec (show r) []) `catch` handler
  where
    handler :: IOError -> IO (Maybe a)
    handler e = do
        T.putStrLn $ "pipe: " `T.append` T.show e
        return Nothing

here :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
here gbs _ mem lhs rhs = (do
  right <-
    case exp2typ mem rhs of
      Nothing -> return Nothing
      Just t -> interpret gbs t mem rhs
  case right of
    Nothing -> return Nothing
    Just (rbs, mr, r) -> do
      left <-
        case exp2typ mr lhs of
          Nothing -> return Nothing
          Just t -> interpret rbs t mr lhs
      case left of
        Nothing -> return Nothing
        Just (lbs, ml, l) -> do
          ro <-
            case vFlatten r of
              -- Tup' (Str' x:_) -> do
                -- x' <- findExecutabre x
                -- case x' of
                --   Nothing -> return $ Just $ show r
                --   Just _ -> cap r
              -- Tup' (Str' _:_) -> cap r
              -- Not sure about this.
              [Str' x] -> return $ Just x
              (Str' _:_) -> cap r
              (Pth' _:_) -> cap r
              -- Str' _          -> cap r
              -- Str' x          -> do
              --   x' <- findExecutabre x
              --   case x' of
              --     Nothing -> return $ Just $ show r
              --     Just _ -> cap r
              -- Tup' (Pth' _:_) -> cap r
              -- Pth' _          -> cap r
              [Out' o h]        -> do
                output <- do
                  -- hSetBuffering out NoBuffering
                  hSetBinaryMode o True
                  B.hGetContents o
                -- copyHandleData out stdout
                -- DEBUG
                -- putStr output
                exitCode <- waitForProcess h
                if exitCode /= ExitSuccess
                then T.putStrLn $ "Process " `T.append` T.show r `T.append` " exited with exit code: " `T.append` T.show exitCode
                else return ()
                -- Bash does this too. Might as well, since I don't want my args to contain newlines.
                return $ Just (T.unwords $ T.words $ T.decodeUtf8With T.lenientDecode output)
              _               -> return Nothing
          let
            rout = fromMaybe "" ro
            exec x xs = do
              -- x' <- findExecutable x
              -- case x' of
              --   Nothing -> return $ Just (lbs, ml, Str' $ rout ++ ' ':(show $ vCollapse $ Tup' $ Str' x:xs))
              --   Just _  -> do
                  let larg = argify (Tup' xs)
                  -- larg <- argIO xs
                  (inp, out, _, landle) <- createProcess (proc (T.unpack x) (map T.unpack larg)) {std_in = CreatePipe, std_out = CreatePipe}
                  case (inp, out) of
                    (Just i, Just o) -> do
                      txt <- do
                        T.hPutStr i rout
                        hSetBinaryMode o True
                        hClose i
                        B.hGetContents o
                      -- putStrLn txt
                      exitcode <- waitForProcess landle
                      if exitcode /= ExitSuccess
                      then T.putStrLn $ "Process " `T.append` x `T.append` (T.unwords larg) `T.append` " exited with exit code: " `T.append` T.show exitcode
                      else return ()
                      return $ Just (lbs, ml, Str' $ T.decodeUtf8With T.lenientDecode txt)
                    _ -> do
                      T.putStrLn $ "Could not create process " `T.append` x `T.append` (T.unwords larg) `T.append` " properly."
                      return Nothing
          -- let lout = show l
          case vFlatten l of
            (Str' x:xs) -> exec x xs
            (Pth' x:xs) -> exec (T.pack x) xs
            -- Tup' (Str' x:xs) -> exec x xs
            -- Tup' (Pth' x:xs) -> exec (T.pack x) xs
            _ -> exec (T.show l) []) `catch` handler
  where
    handler :: IOError -> IO (Maybe a)
    handler e = do
        T.putStrLn $ "here: " `T.append` T.show e
        return Nothing
    cap :: Value -> IO (Maybe T.Text)
    cap v = do
      o <- cmd v
      case o of
        Nothing -> return Nothing
        Just (out, procHand) -> do
          output <- do
            -- hSetBuffering out NoBuffering
            hSetBinaryMode out True
            B.hGetContents out
          -- copyHandleData out stdout
          -- DEBUG
          -- putStr output
          exitCode <- waitForProcess procHand
          if exitCode /= ExitSuccess
          then T.putStrLn $ "Process " `T.append` T.show v `T.append` " exited with exit code: " `T.append` T.show exitCode
          else return ()
          -- Bash does this too. Might as well, since I don't want my args to contain newlines.
          return $ Just (T.unwords $ T.words $ T.decodeUtf8With T.lenientDecode output)

wrt :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
wrt gbs _ mem lhs rhs = do
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
          case vFlatten l of
            -- Str' x -> do
            --   (_, _, _, ph) <- createProcess (proc x []) {std_out = UseHandle file}
            --   _ <- waitForProcess ph `catch` handler2
            --   return $ Just (rbs, mr, Tup' [])
            (Str' x:xs) -> do
              file <- openBinaryFile (T.unpack $ T.unwords $ argify r) WriteMode
              let args = argify (Tup' xs)
              -- args <- argIO xs
              (_, _, _, ph) <- createProcess (proc (T.unpack x) (map T.unpack args)) {std_out = UseHandle file}
              _ <- waitForProcess ph `catch` handler2
              return $ Just (rbs, mr, Tup' [])
            -- Pth' x -> do
            --   file <- openBinaryFile (T.unpack $ T.unwords $ argify r) WriteMode
            --   (_, _, _, ph) <- createProcess (proc x []) {std_out = UseHandle file}
            --   _ <- waitForProcess ph `catch` handler2
            --   return $ Just (rbs, mr, Tup' [])
            (Pth' x:xs) -> do
              file <- openBinaryFile (T.unpack $ T.unwords $ argify r) WriteMode
              let args = argify (Tup' xs)
              -- args <- argIO xs
              (_, _, _, ph) <- createProcess (proc x (map T.unpack args)) {std_out = UseHandle file}
              _ <- waitForProcess ph `catch` handler2
              return $ Just (rbs, mr, Tup' [])
            -- Out' o h -> do
            --   file <- openBinaryFile (T.unpack $ T.unwords $ argify r) WriteMode
            --   copyHandleData o file
            --   _ <- waitForProcess h `catch` handler2
              -- return $ Just (rbs, mr, Tup' [])
            _ -> do
              (T.appendFile (T.unpack $ T.unwords (argify r)) (T.show l)) `catch` handler
              return $ Just (rbs, mr, Tup' [])
  where
    handler :: IOError -> IO ()
    handler e = do
        T.putStrLn $ "write: " `T.append` T.show e
        return ()
    handler2 :: IOError -> IO ExitCode
    handler2 e = do
        T.putStrLn $ "write: " `T.append` T.show e
        return $ ExitFailure 1

apn :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
apn gbs _ mem lhs rhs = do
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
          case vFlatten l of
            -- Str' x -> do
            --   (_, _, _, ph) <- createProcess (proc x []) {std_out = UseHandle file}
            --   _ <- waitForProcess ph `catch` handler2
            --   return $ Just (rbs, mr, Tup' [])
            (Str' x:xs) -> do
              file <- openBinaryFile (T.unpack $ T.unwords $ argify r) AppendMode
              let args = argify (Tup' xs)
              -- args <- argIO xs
              (_, _, _, ph) <- createProcess (proc (T.unpack x) (map T.unpack args)) {std_out = UseHandle file}
              _ <- waitForProcess ph `catch` handler2
              return $ Just (rbs, mr, Tup' [])
            -- Pth' x -> do
            --   file <- openBinaryFile (T.unpack $ T.unwords $ argify r) AppendMode
            --   (_, _, _, ph) <- createProcess (proc x []) {std_out = UseHandle file}
            --   _ <- waitForProcess ph `catch` handler2
            --   return $ Just (rbs, mr, Tup' [])
            (Pth' x:xs) -> do
              file <- openBinaryFile (T.unpack $ T.unwords $ argify r) AppendMode
              let args = argify (Tup' xs)
              -- args <- argIO xs
              (_, _, _, ph) <- createProcess (proc x (map T.unpack args)) {std_out = UseHandle file}
              _ <- waitForProcess ph `catch` handler2
              return $ Just (rbs, mr, Tup' [])
            -- Out' o h -> do
            --   file <- openBinaryFile (T.unpack $ T.unwords $ argify r) WriteMode
            --   copyHandleData o file
            --   _ <- waitForProcess h `catch` handler2
            --   return $ Just (rbs, mr, Tup' [])
            _ -> do
              (appendFile (T.unpack $ T.unwords (argify r)) (show l)) `catch` handler
              return $ Just (rbs, mr, Tup' [])
  where
    handler :: IOError -> IO ()
    handler e = do
        T.putStrLn $ "append: " `T.append` T.show e
        return ()
    handler2 :: IOError -> IO ExitCode
    handler2 e = do
        T.putStrLn $ "append: " `T.append` T.show e
        return $ ExitFailure 1
  --       Just (rbs, mr, r) -> do
  --         lout <- cap l
  --         case lout of
  --           Nothing -> return Nothing
  --           Just lo -> do
  --             (appendFile (unwords (argify r)) lo) `catch` handler
  --             return $ Just (rbs, mr, Tup' [])
  -- where
  --   handler :: IOError -> IO ()
  --   handler e = do
  --       putStrLn $ "here: " ++ show e
  --       return ()

len :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
len gbs _ mem lhs rhs =
  -- left  <-
  --   case exp2typ mem lhs of
  --     Nothing -> return Nothing
  --     Just t -> interpret gbs t mem lhs
  -- case left of
  --   Nothing -> return Nothing
  --   Just (lbs, ml, l) -> do
  if lhs == Operand (Tup [])
  then do
    right <-
      case exp2typ mem rhs of
        Nothing -> return Nothing
        Just t -> interpret gbs t mem rhs
    case right of
      Nothing -> return Nothing
      Just (rbs, mr, r) ->
        case r of
          Int'   b -> return $ Just (rbs, mr, Int' $ toInteger $ length $ show b)
          Flt'   b -> return $ Just (rbs, mr, Int' $ toInteger $ length $ show b)
          Arr' _ b -> return $ Just (rbs, mr, Int' $ toInteger $ length b)
          Tup'   b -> return $ Just (rbs, mr, Int' $ toInteger $ length b)
          -- TODO change this and all other variations to just filter (== '/') or something
          Pth'   b -> return $ Just (rbs, mr, Int' $ toInteger $ length $ pieces (== '/') (T.pack b))
          _        -> return Nothing
  else
    return Nothing

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
            (Int' a, Flt' b) -> return $ Just (rbs, mr, Arr' Tint $ map (Flt') [fromInteger a..b])
            (Flt' a, Int' b) -> return $ Just (rbs, mr, Arr' Tint $ map (Flt') [a..fromInteger b])
            (Flt' a, Flt' b) -> return $ Just (rbs, mr, Arr' Tflt $ map (Flt') [a..b])
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
                    cdir <- canonicalizePath parentA
                    fs <- listDirectory cdir
                    let
                      files = map ((cdir ++) . ('/':)) fs
                      fcycle = cycle files
                    ca <- canonicalizePath a
                    cb <- canonicalizePath b
                    return $ Just (rbs, mr, Arr' Tpth $ map (Pth') (takeWhile (/= cb) (dropWhile (/= ca) fcycle) ++ [cb]))
                  else do
                    T.putStrLn $ "Directory " `T.append` T.pack a `T.append` " does not exist."
                    return Nothing
                else return Nothing                  
            (Tup' [], Pth' b) -> return $ Just (rbs, mr, Pth' $ ".." ++ b)
            (Tup' [], Tup' []) -> return $ Just (rbs, mr, Pth' "..")
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
          line <- T.getLine
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
              case fltMaybe b of
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
              case blnMaybe b of
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
            (Tup' [], Str' (b T.:< T.Empty)) -> return $ Just (rbs, mr, Chr' b)
            _                 -> return Nothing

typ :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
typ gbs _ mem lhs rhs = do
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
            Tup' [] -> return $ Just (rbs, mr, Typ' $ val2typ r)
            _ -> return Nothing

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
            Tup' [] ->
              case r of
                Arr' Tchr cs -> return $ Just (rbs, mr, Str' (T.pack $ chr2str cs))
                _ -> return $ Just (rbs, mr, Str' $ T.show $ vCollapse r)
            _       -> return Nothing
    where
      chr2str [] = []
      chr2str (v:vs) =
        case v of
          Chr' c -> c:chr2str vs
          _      -> []

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
            (Tup' [], Str' b) -> return $ Just (rbs, mr, Pth' $ T.unpack b)
            (Tup' [], Pth' _) -> return $ Just (rbs, mr, r)
            (Tup' [], Arr' Tstr b) -> return $ Just (rbs, mr, Pth' (intercalate "/" $ map (show) b))
            _       -> return Nothing

ift :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
ift gbs etp mem lhs rhs =
  case lhs of
    Operand (Tup []) ->
      case rhs of
        Operand (Tup [predicate, expression]) -> do
          left <- interpret gbs Tbln mem (Operand predicate)
          case left of
            Nothing -> return Nothing
            Just (_, ml, l) ->
              if l == Bln' True
              then interpret True etp ml (Operand expression)
              else do
                -- DEBUG
                -- putStrLn $ show l
                return $ Just (False, ml, Tup' [])
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
els gbs etp mem lhs rhs =
  case lhs of
    Expression _ "if" lleft lright ->
      if exp2typ mem lhs == exp2typ mem rhs || exp2typ mem lhs == Just Tany || exp2typ mem rhs == Just Tany
      then do
        left <- ift gbs etp mem lleft lright
        case left of
          Nothing -> return Nothing
          Just (lbs, ml, l) -> 
            if lbs
            then return $ Just (lbs, ml, l)
            else interpret lbs etp ml rhs
      else
        (T.putStrLn $ "Operator if/else (that are in the same line) "
        `T.append` T.show (flatten lhs) `T.append` " and " `T.append` T.show (flatten rhs) `T.append`
        " output types do not match.") >>=
        \_ -> return Nothing
    Operand (Tup []) ->
      if not gbs
      then interpret gbs etp mem rhs
      else return $ Just (gbs, mem, Tup' [])
    _ -> return Nothing
-- els gbs etp mem lhs rhs = do
  -- left <-
  --   case exp2typ mem lhs of
  --     Just Tbln -> interpret gbs Tbln mem lhs
  --     Just Tany -> interpret gbs Tbln mem lhs
  --     _         -> return Nothing
  -- case left of
  --   Nothing -> return Nothing
  --   Just (lbs, ml, _) ->
  --     if lbs == False
  --     then interpret lbs etp ml rhs
  --     else return $ Just (lbs, ml, Tup' [])

-- Make it just if but it's recursive until the predicate evaluates to false.
whl :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
whl gbs etp mem lhs rhs =
  case lhs of
    Operand (Tup []) ->
      case rhs of
        Operand (Tup [predicate, expression]) -> do
          left <- interpret gbs Tbln mem (Operand predicate)
          case left of
            Just (_, ml, Bln' True) -> do
                iteration <- interpret True etp ml (Operand expression)
                case iteration of
                  Nothing -> return Nothing
                  Just (_, mr, Break) -> return $ Just (True, mr, Arr' Tany [])
                  Just (_, mr, r) -> do
                    final <- whl True etp mr lhs rhs
                    case final of
                      Nothing -> return Nothing
                      Just (fbs, mf, Arr' t f) ->
                        if t == val2typ r || t == Tany
                        then return $ Just (fbs, mf, Arr' t (r:f))
                        else
                          (T.putStrLn $ "while: While loop `" `T.append` T.show expression `T.append` " detected to have different output types.") >>=
                          \_ -> return Nothing
                      Just (_, _, _) -> T.putStrLn "while: branch should not be reachable." >>= \_ -> return Nothing
                        -- if val2typ f == val2typ r || f == Arr' Tany []
                        -- then return $ Just (fbs, mf, Arr' (val2typ r) [r, f])
                        -- else
                        --   (T.putStrLn $ "While loop `" `T.append` T.show expression `T.append` " detected to have different output types.") >>=
                        --   \_ -> return Nothing
            Just (_, ml, Bln' False) -> return $ Just (False, ml, Arr' Tany [])
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
                iteration <- interpret True etp mem lhs
                case iteration of
                  Nothing -> return Nothing
                  Just (_, ml, Break) -> return $ Just (True, ml, Arr' Tany [])
                  Just (_, ml, l) -> do
                    final <- whl True etp ml lhs rhs
                    case final of
                      Nothing -> return Nothing
                      Just (fbs, mf, Arr' t f) ->
                        if t == val2typ l || t == Tany
                        then return $ Just (fbs, mf, Arr' t (l:f))
                        else
                          (T.putStrLn $ "while: While loop `" `T.append` T.show lhs `T.append` " detected to have different output types.") >>=
                          \_ -> return Nothing
                      Just (_, _, _) -> T.putStrLn "while: branch should not be reachable." >>= \_ -> return Nothing
                        -- if val2typ f == val2typ l || f == Arr' Tany []
                        -- then return $ Just (fbs, mf, Arr' (val2typ l) [l, f])
                        -- else
                        --   (T.putStrLn $ "While loop `" `T.append` T.show lhs `T.append` " detected to have different output types.") >>=
                        --   \_ -> return Nothing
                      -- Just (fbs, mf, Tup' f) -> return $ Just (fbs, mf, Tup' (l:f))
                      -- Just (fbs, mf,      f) -> return $ Just (fbs, mf, Tup' [l, f])
        Just (_, mr, Bln' False) -> return $ Just (False, mr, Arr' Tany [])
        _ -> return Nothing

-- Deals with for (initialize, predicate, increment) loops.
frl :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
frl gbs etp mem lhs rhs = do
  case lhs of
    Operand (Tup []) ->
      case rhs of
        Operand (Tup [Tup predicate, expression]) ->
          case parse mem predicate of
            Expression _ "," (Expression _ "," ini pdc) inc -> do
              iniout <- interpret gbs etp (newscope mem) ini
              case iniout of
                Nothing -> return Nothing
                Just (ibs, im, _) ->
                  (whl ibs etp im (Expression 0 "," (Operand expression) inc) pdc) >>=
                  (\out -> case out of {Nothing -> return Nothing; Just (obs, om, ov) -> return $ Just (obs, drop 1 om, ov)})
            _ ->
              (T.putStrLn $ "for: Invalid for loop syntax at `" `T.append` T.show (parse mem predicate) `T.append` "`.")
              >>= (\_ -> return Nothing)
        _ -> (T.putStrLn $ "for: Invalid for loop syntax at `" `T.append` T.show (flatten rhs) `T.append` "`.") >>= (\_ -> return Nothing)
    _ -> do
      case rhs of
        Expression _ "," (Expression _ "," ini pdc) inc -> do
          iniout <- interpret gbs etp (newscope mem) ini
          case iniout of
            Nothing -> return Nothing
            Just (ibs, im, _) ->
              (whl ibs etp im (Expression 0 "," lhs inc) pdc) >>=
              (\out -> case out of {Nothing -> return Nothing; Just (obs, om, ov) -> return $ Just (obs, drop 1 om, ov)})
        _ ->
          (T.putStrLn $ "for: Invalid for loop syntax at `" `T.append` T.show (flatten rhs) `T.append` "`.")
          >>= (\_ -> return Nothing)

-- Deals with foreach (variable, list of values) loops.
fre :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
fre gbs _ mem lhs rhs =
  case lhs of
    Operand (Tup []) ->
      case rhs of
        Operand (Tup [predicate, expression]) -> do
          right <- interpret gbs (Ttup [Tstr, Tany]) mem (Operand predicate)
          case right of
            Just (rbs, mr, Tup' [Str' vname, val]) -> do
              iterator <-
                case val of
                  Tup'   tup -> return tup
                  Arr' _ arr -> return arr
                  Pth' path  -> do
                    exists  <- doesDirectoryExist path
                    if exists
                    then do
                      cdir <- canonicalizePath path
                      paths <- listDirectory cdir
                      return (map (Pth' . (cdir ++) . ('/':)) paths)
                    else return [Pth' path]
                  _          -> return [val]
              let
                body =
                  foldl'
                  (
                    \acc i -> do
                      x <- acc
                      case x of
                        Nothing -> return Nothing
                        Just (b, m, ls) -> do
                          left <- interpret b Tany (insertVar vname i $ newscope m) (Expression 0 "," (Operand $ Tup []) (Operand expression))
                          case left of
                            Nothing -> return Nothing
                            Just (b', m', r) -> return $ Just (b', drop 1 m', r:ls)
                  )
                  (return $ Just (rbs, mr, []))
                  iterator
              body' <- body
              case body' of
                Nothing -> return Nothing
                Just (b, m, vs@(v:_)) ->
                  if dupes (map val2typ vs)
                  then return $ Just (b, m, Arr' (val2typ v) $ reverse vs)
                  else
                    (T.putStrLn $ "foreach: For each loop `" `T.append` T.show expression `T.append` "` found to have different output types.") >>=
                    \_ -> return Nothing
                Just (b, m, []) -> return $ Just (b, m, Arr' Tany $ [])
            _ -> return Nothing
        _ -> return Nothing
    _ -> do
      right <- interpret gbs (Ttup [Tstr, Tany]) mem rhs
      case right of
        Just (rbs, mr, Tup' [Str' vname, val]) -> do
          iterator <-
            case val of
              Tup'   tup -> return tup
              Arr' _ arr -> return arr
              Pth' path  -> do
                exists <- doesDirectoryExist path
                if exists
                then do
                  cdir <- canonicalizePath path
                  paths <- listDirectory cdir
                  return (map (Pth' . (cdir ++) . ('/':)) paths)
                else return [Pth' path]
              _          -> return [val]
          let
            body =
              foldl'
              (
                \acc i -> do
                  x <- acc
                  case x of
                    Nothing -> return Nothing
                    Just (b, m, ls) -> do
                      left <- interpret b Tany (insertVar vname i $ newscope m) (Expression 0 "," (Operand $ Tup []) lhs)
                      case left of
                        Nothing -> return Nothing
                        Just (b', m', r) -> return $ Just (b', drop 1 m', r:ls)
              )
              (return $ Just (rbs, mr, []))
              iterator
          body' <- body
          case body' of
            Nothing -> return Nothing
            -- Just (b, m, vs) -> return $ Just (b, m, Tup' $ reverse vs)
            Just (b, m, vs@(v:_)) ->
              if dupes (map val2typ vs)
              then return $ Just (b, m, Arr' (val2typ v) $ reverse vs)
              else
                (T.putStrLn $ "foreach: For each loop `" `T.append` T.show lhs `T.append` "` found to have different output types.") >>=
                \_ -> return Nothing
            Just (b, m, []) -> return $ Just (b, m, Arr' Tany $ [])
        _ -> return Nothing

brk :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
brk gbs _ mem lhs rhs =
  if lhs == (Operand $ Tup []) && rhs == (Operand $ Tup [])
  then return $ Just (gbs, mem, Break)
  else (putStrLn "Break statement's left-hand side and/or right-hand side is/are not empty.") >>= \_ -> return Nothing

ret :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
ret gbs etp mem lhs rhs =
  if lhs == (Operand $ Tup [])
  then interpret gbs etp mem rhs
  else (putStrLn "Return statement's left-hand side is not empty.") >>= \_ -> return Nothing

-- Get deferred type from the = operator. It's provided in the etp input variable.
var :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
var gbs etp mem lhs rhs =
  if lhs == (Operand $ Tup [])
  then
    case rhs of
      Operand (Wrd vname) ->
        if not $ hasTany etp
        then return $ Just (gbs, insertBlankVar vname etp mem, Str' vname)
        else
          (T.putStrLn $ "Can't defer the type of the let operation's assigned value. Type signature given is `" `T.append` T.show etp `T.append` "`.") >>=
          \_ -> return Nothing
      Operand (Tup [Typ tin, Wrd vname]) ->
        if tin == etp || etp == Tany
        then return $ Just (gbs, insertBlankVar vname etp mem, Str' vname)
        else
          (T.putStrLn $ "Let statement with defined type `" `T.append` T.show tin `T.append` "` does not match the derived type on the right `" `T.append` T.show etp `T.append` "`.") >>=
          \_ -> return Nothing
      Operand (Tup [tin, Wrd vname]) ->
        if compoundType tin == Just etp || etp == Tany
        then return $ Just (gbs, insertBlankVar vname etp mem, Str' vname)
        else
          (T.putStrLn $ "Let statement with defined type `" `T.append` T.show tin `T.append` "` does not match the derived type on the right `" `T.append` T.show etp `T.append` "`.") >>=
          \_ -> return Nothing
      -- Catching variables already defined.
      Operand (Var vname) -> var gbs etp mem lhs (Operand (Wrd vname))
      Operand (Tup [Typ tin, Var vname]) -> var gbs etp mem lhs (Operand (Tup [Typ tin, Wrd vname]))
      Operand (Tup [tin, Var vname]) -> var gbs etp mem lhs (Operand (Tup [tin, Wrd vname]))
      _ -> (T.putStrLn $ "Let statement can't parse right-hand side `" `T.append` T.show (flatten rhs) `T.append` "`") >>= \_ -> return Nothing
  else (T.putStrLn "Let statement's left-hand side is not empty.") >>= \_ -> return Nothing

opr :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
opr _ _ mem _ _ =
  T.putStrLn "The current implementation of Turtle Shell Scripting Language does not allow for operation declaration without definition yet." >>=
  \_ -> putStrLn (show (map (M.filter (\x -> case x of Op _ _ -> True ; _ -> False)) mem)) >>=
  \_ -> return Nothing

-- Eval right, then check left if it needs evaluation, if it doesn't then check type of left, then assign (or not).
-- Maybe I'll add support for pattern matching here later.
asn :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
asn gbs _ mem lhs rhs = do
  case lhs of
    -- Implement hole
    Operand (Wrd "_") ->
      interpret gbs Tany mem rhs >>=
      \rout ->
        case rout of
          Nothing -> return Nothing
          Just (rbs, mr, _) -> return $ Just (rbs, mr, Tup' [])
    -- To catch things already defined.
    Operand (Var vname) -> asn gbs Tany mem (Operand (Wrd vname)) rhs
    Operand (Opr _ vname) -> asn gbs Tany mem (Operand (Wrd vname)) rhs
    Operand (Wrd vname) -> do
      right <-
        case exp2typ mem rhs of
          Nothing -> return Nothing
          Just t ->
            case getMem mem vname of
              Just (Val v) ->
                case v of
                  Left vt ->
                    if vt == t
                    then
                      interpret gbs t mem rhs
                    else do
                      T.putStrLn $ "Can't assign to  variable `" `T.append` vname `T.append` "`. (Type mismatch)"
                      return Nothing
                  Right l ->
                    if val2typ l == t
                    then
                      interpret gbs t mem rhs
                    else do
                      T.putStrLn $ "Can't assign to  variable `" `T.append` vname `T.append` "`. (Type mismatch)"
                      return Nothing
              _ -> do
                T.putStrLn $ "Can't assign to  variable `" `T.append` vname `T.append` "`. (Type mismatch)"
                return Nothing
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) ->
          case getMem mr vname of
            Just (Val v) ->
              case v of
                Left t ->
                  if t == val2typ r
                  then do
                    -- DEBUG
                    -- putStrLn $ show $ map (M.filter (\d -> case d of {Op _ _ -> False; Val _ -> True;})) (updateVar vname r mr)
                    return $ Just (rbs, updateVar vname r mr, Tup' [])
                  else do
                    T.putStrLn $ "Can't assign a value of type `" `T.append` T.show (val2typ r) `T.append` "` to variable `" `T.append` vname `T.append` "` of type `" `T.append` T.show t `T.append` "`"
                    return Nothing
                Right x ->
                  if val2typ x == val2typ r
                  then do
                    -- DEBUG
                    -- putStrLn $ show $ map (M.filter (\d -> case d of {Op _ _ -> False; Val _ -> True;})) (updateVar vname r mr)
                    return $ Just (rbs, updateVar vname r mr, Tup' [])
                  else do
                    T.putStrLn $ "Can't assign a value of type `" `T.append` T.show (val2typ r) `T.append` "` to variable `" `T.append` vname `T.append` "` of type `" `T.append` T.show (val2typ x) `T.append` "`"
                    return Nothing
            _ -> do
              T.putStrLn $ "Can't assign `" `T.append` T.show r `T.append` "` to  variable `" `T.append` vname `T.append` "`."
              return Nothing
    Expression _ "set" lset rset -> do
      if lset == Operand (Tup [])
      then do
        right <-
            case exp2typ mem rhs of
            Nothing -> return Nothing
            Just t -> interpret gbs t mem rhs
        case right of
          Nothing -> return Nothing
          Just (rbs, mr, r) ->
            case rset of
              Operand (Str nam) -> do
                setEnv (T.unpack nam) (show r) `catch` (\(e :: IOError) -> T.putStrLn (T.show e) >>= \_ -> return ())
                return $ Just (rbs, mr, Tup' [])
              Operand (Wrd nam) -> do
                setEnv (T.unpack nam) (show r) `catch` (\(e :: IOError) -> T.putStrLn (T.show e) >>= \_ -> return ())
                return $ Just (rbs, mr, Tup' [])
              Operand (Var nam) ->
                case getMem mr nam of
                  Just (Val (Right nam')) -> do
                    let nam'' = show nam'
                    setEnv nam'' (show r) `catch` (\(e :: IOError) -> T.putStrLn (T.show e) >>= \_ -> return ())
                    return $ Just (rbs, mr, Tup' [])
                  _ ->
                    T.putStrLn "Variable used in set operation not defined yet." >>=
                    \_ -> return Nothing
              _ ->
                T.putStrLn "Set operation's middle part (name) needs to be evaluated." >>=
                \_ -> return Nothing
      else
        T.putStrLn "Set operation's left-hand side is not empty." >>=
        \_ -> return Nothing
    Expression _ "let" llet@(Operand (Tup [tp, Str vname])) rlet -> do
      right <-
        case compoundType tp of
          Nothing -> return Nothing
          Just vt ->
            case exp2typ mem rhs of
                Nothing -> return Nothing
                Just t ->
                  if vt == t
                  then
                    interpret gbs t mem rhs
                  else do
                    T.putStrLn $ "Can't assign to  variable `" `T.append` vname `T.append` "`. (Type mismatch)"
                    return Nothing
        -- case exp2typ mem rhs of
          -- Nothing -> return Nothing
          -- Just t -> interpret gbs t mem rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) -> do
          -- This feels so icky I don't know why.
          left <- var rbs (val2typ r) mr llet rlet
          case left of
            Just (lbs, ml, Str' l) -> return $ Just (lbs, insertVar l r ml, Tup' [])
            _ -> return Nothing
    Expression _ "let" llet rlet -> do
      right <-
        case exp2typ mem rhs of
          Nothing -> return Nothing
          Just t -> interpret gbs t mem rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, r) -> do
          -- This feels so icky I don't know why.
          left <- var rbs (val2typ r) mr llet rlet
          case left of
            Just (lbs, ml, Str' l) -> return $ Just (lbs, insertVar l r ml, Tup' [])
            _ -> return Nothing
    -- opr will exist entirely within this branch for now.
    Expression _ "opr" lopr ropr ->
      if lopr == Operand (Tup [])
      -- Parse the inputs and then pack it into a defined struct,
      -- which looks like this: Defined Word8 (VarTree, Expression) Type
      --                                       ^ ropr   ^ rhs       ^ exp2typ rhs
      -- and VarTree looks like this: VarTree = VarName String | VarGroup [VarTree]
      then do
        let
          typish :: Token -> Bool
          typish tok = compoundType tok /= Nothing
          -- This one doesn't check for invalid things yet. (stuff that aren't types or names)
          splitOp :: [Token] -> Maybe ([Token], T.Text, [Token])
          -- splitOp (Wrd left:Wrd opname:right) = Just ([Wrd left], opname, right)
          -- splitOp (Var left:Wrd opname:right) = Just ([Wrd left], opname, right)
          splitOp (Wrd opname:right) = Just ([], opname, right)
          splitOp (Var opname:right) = Just ([], opname, right)
          splitOp (Opr _ opname:right) = Just ([], opname, right)
          splitOp (Typ x1:Wrd x2:xs) =
            case splitOp xs of
              Nothing -> Nothing
              Just (l, s, r) -> Just (Typ x1:Wrd x2:l, s, r)
          -- splitOp [] = Nothing
          splitOp (Tup x1:Wrd x2:xs) =
            if typish (Tup x1)
            then
              case splitOp xs of
                Nothing -> Nothing
                Just (l, s, r) -> Just (Tup x1:Wrd x2:l, s, r)
            else
              Just ([Tup x1], x2, xs)
          splitOp (Arr x1:Wrd x2:xs) =
            if typish (Tup x1)
            then
              case splitOp xs of
                Nothing -> Nothing
                Just (l, s, r) -> Just (Tup x1:Wrd x2:l, s, r)
            else
              Nothing
          -- splitOp (x:xs) =
          --   case splitOp xs of
          --     Nothing -> Nothing
          --     Just (l, s, r) -> Just (x:l, s, r)
          splitOp (x1:Var x2:xs) = splitOp (x1:Wrd x2:xs)
          splitOp (x1:Opr _ x2:xs) = splitOp (x1:Wrd x2:xs)
          splitOp _ = Nothing
          -- I don't think I need this, seems jank too.
          -- splitOp xs =
          --   case splitWith (\x -> case x of {Opr _ _ -> True; _ -> False}) xs of
          --     (left, (Opr _ opname:right)) -> Just (left, opname, right)
          --     _ -> Nothing
          tok2pair :: [Token] -> Maybe (VarTree, Type)
          tok2pair (tok:Var v:ts) = tok2pair (tok:Wrd v:ts)
          tok2pair (tok:Wrd v:ts) =
            case compoundType tok of
              -- If it's not a compound type, it shouldn't have a string following it.
              Nothing -> Nothing
              Just t ->
                case tok2pair ts of
                  -- The first variation could be Ttup (Ttup Tany:[]), but the second one can't.
                  Just (VarGroup vg, Ttup tp) -> Just (VarGroup (VarName v:vg), Ttup (t:tp))
                  Just (VarName vn, tp) -> Just (VarGroup [VarName v, VarName vn], Ttup [t, tp])
                  -- Can't have a vargroup associate with only one non-tuple type, that's illegal.
                  _ -> Nothing
          tok2pair (Tup tup:ts) =
            case tok2pair tup of
              Nothing -> Nothing
              Just (vt, t) ->
                case tok2pair ts of
                  -- This changes (a (b c)) -> (a b c)
                  -- The first variation could be Ttup (Ttup Tany:[]), but the second one can't.
                  Just (VarGroup vg, Ttup tp) -> Just (VarGroup (vt:vg), Ttup (t:tp))
                  Just (VarName vn, tp) -> Just (VarGroup [vt, VarName vn], Ttup [t, tp])
                  -- Can't have a vargroup associate with only one non-tuple type, that's illegal.
                  _ -> Nothing
          tok2pair [] = Just (VarGroup [], Ttup [])
          tok2pair _ = Nothing
          zipName :: VarTree -> Type -> [(T.Text, Data)]
          zipName vt t =
            case vt of
              VarName vn -> [(vn, Val $ Left t)]
              VarGroup vg ->
                case t of
                  Ttup tt -> concat $ zipWith (zipName) vg tt
                  _       -> undefined -- This would mean the parsing was wrong, and would therefore fail.
          -- Creates new expression tree from a fake memory used only for parsing
          -- the tree with the operator name in scope.
          -- Used if the operator doesn't already exist.
          -- newrhs :: Word8 -> String -> Expression
          -- newrhs rank opname =
          --   parse
          --   (
          --     insertMem opname (Op rank (M.empty, Nothing)) mem
          --   )
          --   (flatten rhs)
        -- case exp2typ mem rhs of
        --   Nothing -> return Nothing
        --   Just t ->
        case ropr of
          -- No expressions due to preprocessing.
          Operand (Opr _ opname) ->
            case getMem mem opname of
              -- do not insert duplicate operator data
              Just (Op rank _) ->
                case exp2typ mem (rhs) of
                  Nothing -> do
                    T.putStrLn $ "Can't derive the type of the defined operator `" `T.append` opname `T.append` "`."
                    return Nothing
                  Just t ->
                    return $ Just
                    (
                      gbs,
                      insertMem
                      opname
                      (
                        Op rank
                        (insertOp
                          (Ttup [], Ttup [])
                          (
                            Defined
                            (fromIntegral $ length mem)
                            (VarGroup [VarGroup [], VarGroup []], rhs)
                            t
                          )
                          (getTopOpMap mem opname)
                        )
                      )
                      mem,
                      Tup' []
                    )
              _ -> do
                T.putStrLn $ "Operator named `" `T.append` opname `T.append` "` does not exist, but is marked as an operator by the tokenizer."
                return Nothing
          Operand (Wrd opname) ->
            case exp2typ mem (rhs) of
              Nothing -> do
                T.putStrLn $ "Can't derive the type of the defined operator `" `T.append` opname `T.append` "`."
                return Nothing
              Just t ->
                return $ Just
                (
                  gbs,
                  insertMem
                  opname
                  (
                    Op 17
                    (insertOp
                      (Ttup [], Ttup [])
                      (
                        Defined
                        (fromIntegral $ length mem)
                        (VarGroup [VarGroup [], VarGroup []], rhs)
                        t
                      )
                      (M.empty, Nothing)
                    )
                  )
                  mem,
                  Tup' []
                )
          Operand (Tup [Int rank, Wrd opname]) ->
            case exp2typ mem (rhs) of
              Nothing -> do
                T.putStrLn $ "Can't derive the type of the defined operator `" `T.append` opname `T.append` "`."
                return Nothing
              Just t ->
                return $ Just
                (
                  gbs,
                  insertMem
                  opname
                  (
                    Op (fromInteger rank)
                    (insertOp
                      (Ttup [], Ttup [])
                      (
                        Defined
                        (fromIntegral $ length mem)
                        (VarGroup [VarGroup [], VarGroup []], rhs)
                        t
                      )
                      (M.empty, Nothing)
                    )
                  )
                  mem,
                  Tup' []
                )
          -- Needs to not exist yet since the rank is specified.
          Operand (Tup (Int rank:ts)) ->
            case splitOp ts of
              Nothing -> do
                T.putStrLn $ "Can't parse the inputs of the defined operator `" `T.append` T.show (Tup ts) `T.append` "`."
                return Nothing
              Just (l, s, r) -> do
                -- DEBUG
                -- putStrLn $ show (l, s, r)
                case (tok2pair l, tok2pair r) of
                  (Just (ln, lt), Just (rn, rt)) ->
                    case getMem mem s of
                      -- do not insert duplicate operator data
                      Nothing ->
                        case exp2typ (M.fromList (zipName (vtCollapse $ VarGroup [ln, rn]) (tCollapse $ Ttup [lt, rt])):mem) (rhs) of
                          Nothing -> do
                            T.putStrLn $ "Can't derive the type of the defined operator `" `T.append` T.show (Tup ts) `T.append` "`."
                            return Nothing
                          Just t ->
                            return $ Just
                            (
                              gbs,
                              insertMem
                              s
                              (
                                Op (fromInteger rank)
                                (insertOp
                                  (tCollapse lt, tCollapse rt)
                                  (
                                    Defined
                                    (fromIntegral $ length mem)
                                    (vtCollapse $ VarGroup [ln, rn], rhs)
                                    t
                                  )
                                  (M.empty, Nothing)
                                )
                              )
                              mem,
                              Tup' []
                            )
                      _ ->
                        (T.putStrLn $ "Binding for operation named `" `T.append` s `T.append` "` already exists.") >>=
                        \_ -> return Nothing
                  _ ->
                    (T.putStrLn $ "Can't parse operator names/types in declaration `" `T.append` T.show ts `T.append` "`.") >>=
                    \_ -> return  Nothing
          Operand (Tup ts) ->
            case splitOp ts of
              Nothing -> do
                T.putStrLn $ "Can't parse the inputs of the defined operator `" `T.append` T.show (Tup ts) `T.append` "`."
                return Nothing
              Just (l, s, r) ->
                case (tok2pair l, tok2pair r) of
                  (Just (ln, lt), Just (rn, rt)) ->
                    case getMem mem s of
                      -- do not insert duplicate operator data
                      Just (Op rank _) ->
                        case exp2typ (M.fromList (zipName (vtCollapse $ VarGroup [ln, rn]) (tCollapse $ Ttup [lt, rt])):mem) (rhs) of
                          Nothing -> do
                            T.putStrLn $ "Can't derive the output type of the operator `" `T.append` s `T.append` "` being defined."
                            return Nothing
                          Just t ->
                            return $ Just
                            (
                              gbs,
                              insertMem
                              s
                              (
                                Op rank
                                (insertOp
                                  (tCollapse lt, tCollapse rt)
                                  (
                                    Defined
                                    (fromIntegral $ length mem)
                                    (vtCollapse $ VarGroup [ln, rn], rhs)
                                    t
                                  )
                                  (getTopOpMap mem s)
                                )
                              )
                              mem,
                              Tup' []
                            )
                      Nothing ->
                        case exp2typ (M.fromList (zipName (vtCollapse $ VarGroup [ln, rn]) (tCollapse $ Ttup [lt, rt])):mem) rhs of
                          Nothing -> do
                            T.putStrLn $ "Can't derive the output type of the operator `" `T.append` s `T.append` "` being defined."
                            return Nothing
                          Just t ->
                            return $ Just
                            (
                              gbs,
                              insertMem
                              s
                              (
                                Op (fromInteger 17)
                                (insertOp
                                  (tCollapse lt, tCollapse rt)
                                  (
                                    Defined
                                    (fromIntegral $ length mem)
                                    (vtCollapse $ VarGroup [ln, rn], rhs)
                                    t
                                  )
                                  (M.empty, Nothing)
                                )
                              )
                              mem,
                              Tup' []
                            )
                      _ ->
                        (T.putStrLn $ "Binding for operation named `" `T.append` s `T.append` "` already exists as a variable.") >>=
                        \_ -> return Nothing
                  _ ->
                    (T.putStrLn $ "Can't parse operator names/types in declaration `" `T.append` T.show ts `T.append` "`.") >>=
                    \_ -> return  Nothing
          expr@(Expression _ _ _ _)  -> do
            -- DEBUG
            -- T.putStrLn $ T.show $ flatten expr
            asn gbs Tany mem (Expression 4 "opr" (Operand $ Tup []) (Operand $ Tup $ flatten expr)) rhs
          _ ->
            (T.putStrLn $ "Operator construction can't be parsed.") >>=
            \_ -> return Nothing
      else
        T.putStrLn "Opr statement's left-hand side is not empty." >>=
        \_ -> return Nothing
    _ -> do
      T.putStrLn "Bad syntax on `=` operator."
      return Nothing

-- Unsure if I should use cap or cmd here. I used cap so it would be able to be used like $()
exe :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
exe gbs _ mem lhs rhs =
  if lhs == Operand (Tup [])
  then do
    right <-
      case exp2typ mem rhs of
        Nothing -> return Nothing
        Just t -> interpret gbs t mem rhs
    case right of
      Nothing -> return Nothing
      Just (rbs, mr, r) ->
        (cap r) >>=
        (\out -> return $ maybe Nothing (\xs -> Just (rbs, mr, Arr' Tstr (map Str' xs))) out)
  else do
  -- I just copied "with" and replaced cmd with cap.
    left <- interpret gbs Tany mem lhs
    case left of
      Nothing -> return Nothing
      Just (lbs, ml, Arr' (Ttup [Tstr, Tstr]) l) -> do
        right <-
          case exp2typ mem rhs of
            Nothing -> return Nothing
            Just t -> interpret lbs t ml rhs
        case right of
          Nothing -> return Nothing
          Just (rbs, mr, r) -> do
            oldenv <- getEnvironment
            out <-
              do
                _ <-
                  sequence $ map
                  (
                    \i ->
                      case i of
                        Tup' [Str' n, Str' v] -> setEnv (T.unpack n) (T.unpack v)
                        _ -> return ()
                  ) l
                cap r
              `catch`
              (
                \(e :: IOError) -> do
                  T.putStrLn $ "cmd: " `T.append` T.show e
                  return Nothing
              )
            _ <-
              do
                newenv <- getEnvironment
                _ <- sequence $ map (\(n, _) -> unsetEnv n) newenv
                _ <- sequence $ map (\(n, v) -> setEnv n v) oldenv
                return ()
              `catch`
              (\(e :: IOError) -> T.putStrLn $ "cmd: " `T.append` T.show e)
            return $ maybe Nothing (\xs -> Just (rbs, mr, Arr' Tstr (map Str' xs))) out
      _ -> return Nothing
    -- return Nothing
  where
    cap :: Value -> IO (Maybe [T.Text])
    cap v = do
      o <- cmd v
      case o of
        Nothing -> return Nothing
        Just (out, procHand) -> do
          output <- do
            -- hSetBuffering out NoBuffering
            hSetBinaryMode out True
            B.hGetContents out
          -- copyHandleData out stdout
          -- DEBUG
          -- putStr output
          exitCode <- waitForProcess procHand
          if exitCode /= ExitSuccess
          then T.putStrLn $ "Process " `T.append` T.show v `T.append` " exited with exit code: " `T.append` T.show exitCode
          else return ()
          -- Bash does this too. Might as well, since I don't want my args to contain newlines.
          return $ Just (T.words $ T.decodeUtf8With T.lenientDecode output)

-- Also highly inefficient.
with :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
with gbs _ mem lhs rhs = do
  left <- interpret gbs Tany mem lhs
  case left of
    Nothing -> return Nothing
    Just (lbs, ml, l) -> do
      right <-
        case exp2typ mem rhs of
          Nothing -> return Nothing
          Just t -> interpret lbs t ml rhs
      case right of
        Nothing -> return Nothing
        Just (rbs, mr, Arr' (Ttup [Tstr, Tstr]) r) -> do
          oldenv <- getEnvironment
          _ <-
            do
              _ <-
                sequence $ map
                (
                  \i ->
                    case i of
                      Tup' [Str' n, Str' v] -> setEnv (T.unpack n) (T.unpack v)
                      _ -> return ()
                ) r
              cmd l
            `catch`
            (\(e :: IOError) -> T.putStrLn ("with: " `T.append` T.show e) >>= \_ -> return Nothing)
          _ <-
            do
              newenv <- getEnvironment
              _ <- sequence $ map (\(n, _) -> unsetEnv n) newenv
              _ <- sequence $ map (\(n, v) -> setEnv n v) oldenv
              return ()
            `catch`
            (\(e :: IOError) -> T.putStrLn $ "with: " `T.append` T.show e)
          return $ Just (rbs, mr, Tup' [])
        _ -> return Nothing

set :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
set _ _ _ _ _ =
  T.putStrLn "Naked set doesn't exist. And won't exist." >>=
  \_ -> return Nothing
-- For unsetting environments for this session
unset :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
unset gbs _ mem lhs rhs =
  if lhs == Operand (Tup [])
  then do
    right <- interpret gbs Tany mem rhs
    case right of
      Nothing -> return Nothing
      Just (rbs, mr, r) -> do
        unsetEnv (show r) `catch` (\(e :: IOError) -> T.putStrLn ("unset: " `T.append` T.show e) >>= \_ -> return ())
        return $ Just (rbs, mr, Tup' [])
  else
    T.putStrLn "Unset operation's left-hand side is not empty." >>=
    \_ -> return Nothing

get :: Bool -> Type -> Memory -> Expression -> Expression -> IO (Maybe (Bool, Memory, Value))
get gbs _ mem lhs rhs =
  if lhs == Operand (Tup [])
  then do
    right <- interpret gbs Tany mem rhs
    case right of
      Nothing -> return Nothing
      Just (rbs, mr, r) -> do
        out <- getEnv (show r) `catch` (\(e :: IOError) -> T.putStrLn ("get: " `T.append` T.show e) >>= \_ -> return "")
        return $ case out of {"" -> Nothing; _ -> Just (rbs, mr, Str' $ T.pack out)}
  else
    T.putStrLn "Unset operation's left-hand side is not empty." >>=
    \_ -> return Nothing
