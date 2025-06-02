module Interpret.Operators where

import Interpret.Data
import Interpret.Evaluate
import Data.Maybe
import Data.Char
import System.Directory
-- import System.Process

add :: Memory -> Expression -> Expression -> IO (Maybe (Memory, Value))
add mem lhs rhs = do
  left  <- interpret (Ttup $ tCollapse $ fromMaybe [Tany] $ exp2typs mem lhs) mem lhs
  right <-
    case left of
      Nothing -> return Nothing
      Just (ml, _) -> interpret (Ttup $ tCollapse $ fromMaybe [Tany] $ exp2typs ml  rhs) mem rhs
  case left of
    Nothing -> return Nothing
    Just (_, l) ->
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
                (Int' a, Chr' b)       -> Int' $ a + (toInteger $ ord b)
                (Chr' a, Int' b)       -> Chr' $ chr $ (ord a) + (fromInteger b)
                (Chr' a, Chr' b)       -> Chr' $ chr $ (ord a) + (ord b)
                (Bln' a, Bln' b)       -> Bln' $ a || b
                -- (Fmt' a, Fmt' b)       -> Fmt' $ a ++ b
                -- (Fmt' a, Str' b)       -> Fmt' $ a ++ [Left b]
                -- (Str' a, Fmt' b)       -> Fmt' $ Left a:b
                -- (Fmt' a, b)            -> Fmt' $ a ++ [Left $ show b]
                -- (a, Fmt' b)            -> Fmt' $ (Left $ show a):b
                (Str' a, b)            -> Str' $ a ++ show b
                (a, Str' b)            -> Str' $ (show a) ++ b
                (Arr' t1 a, Arr' t2 b) ->
                  if t1 == t2
                  then Arr' t1 (a ++ b)
                  else Tup' [Arr' t1 a, Arr' t2 b]
                (Arr' t a, b)          -> Arr' t $ map (addv b) a
                (a, Arr' t b)          -> Arr' t $ map (addv a) b
                (Tup' a, b)            -> Tup'   $ a ++ [b]
                (a, Tup' b)            -> Tup'   $ a:b
                (a, b)                 -> Tup'     [a, b]
          in
            return $ Just (mr, addv l r)

-- for multiply, sequence it. (sequence map (* rhs) lhs)
--
cd :: Memory -> Expression -> Expression -> IO (Maybe (Memory, Value))
cd mem _ rhs = do
  right  <- interpret (Ttup $ tCollapse $ fromMaybe [Tany] $ exp2typs mem rhs) mem rhs
  case right of
    Just (mem', r) -> do
      let pth = show r
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
      \_ -> return Nothing

-- setenv :: Memory -> Expression -> Expression -> IO (Maybe (Memory, Value))
-- setenv mem _ rhs = do
--   right  <- interpret (Ttup $ tCollapse $ fromMaybe [Tany] $ exp2typs mem rhs) mem rhs
--   something
--   case right of
--     Just (mem', r) -> return $ Just (mem', Tup' [])
