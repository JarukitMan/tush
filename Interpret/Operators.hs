module Interpret.Operators where

import Misc
import Interpret.Data
import Interpret.Evaluate
import Data.Maybe
import Data.List(intercalate)
import qualified Data.Char as C
import System.Directory
-- import System.Directory.Internal
import System.Process
import System.IO
import System.Exit
import Control.Exception
-- import System.IO.Error
-- import System.Environment

add :: Type -> Memory -> Expression -> Expression -> IO (Maybe (Memory, Value))
add _ mem lhs rhs = do
  left  <- interpret (Ttup $ tCollapse $ fromMaybe [Tany] $ exp2typs mem lhs) mem lhs
  case left of
    Nothing -> return Nothing
    Just (ml, l) -> do
      right <- interpret (Ttup $ tCollapse $ fromMaybe [Tany] $ exp2typs ml  rhs) mem rhs
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
              _ -> return $ Just (mr, addv l r)

sub :: Type -> Memory -> Expression -> Expression -> IO (Maybe (Memory, Value))
sub _ mem lhs rhs = do
  left  <- interpret (Ttup $ tCollapse $ fromMaybe [Tany] $ exp2typs mem lhs) mem lhs
  case left of
    Nothing -> return Nothing
    Just (ml, l) -> do
      right <- interpret (Ttup $ tCollapse $ fromMaybe [Tany] $ exp2typs ml  rhs) mem rhs
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
              Just result -> return $ Just (mr, result)


mpy :: Type -> Memory -> Expression -> Expression -> IO (Maybe (Memory, Value))
mpy _ mem lhs rhs = do
  left  <- interpret (Ttup $ tCollapse $ fromMaybe [Tany] $ exp2typs mem lhs) mem lhs
  case left of
    Nothing -> return Nothing
    Just (ml, l) -> do
      right <- interpret (Ttup $ tCollapse $ fromMaybe [Tany] $ exp2typs ml  rhs) mem rhs
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
                (Arr' t a, Int' b)     -> Just $ Arr' (Tarr t) $ map (Arr' t) (replicate (fromInteger b) a)
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
              Just result -> return $ Just (mr, result)

dvd :: Type -> Memory -> Expression -> Expression -> IO (Maybe (Memory, Value))
dvd _ mem lhs rhs = do
  left  <- interpret (Ttup $ tCollapse $ fromMaybe [Tany] $ exp2typs mem lhs) mem lhs
  case left of
    Nothing -> return Nothing
    Just (ml, l) -> do
      right <- interpret (Ttup $ tCollapse $ fromMaybe [Tany] $ exp2typs ml  rhs) mem rhs
      case right of
        Nothing -> return Nothing
        Just (mr, r) ->
          let
            dvdv x y =
              case (x, y) of
                (Int' a, Int' b)       -> Just $ Int' $ a `div` b
                (Int' a, Flt' b)       -> Just $ Flt' $ (fromInteger a) / b
                (Flt' a, Int' b)       -> Just $ Flt' $ a / (fromInteger b)
                (Flt' a, Flt' b)       -> Just $ Flt' $ a / b
          --       (Int' a, Chr' b)       -> Just $ Int' $ a * (toInteger $ C.ord b)
          --       (Chr' a, Int' b)       -> Just $ Str' $ replicate (fromInteger b) a
          --       (Bln' a, Bln' b)       -> Just $ Bln' $ a && b
          --       (Str' a, Int' b)       -> Just $ Str' $ concat $ replicate (fromInteger b) a
          --       (Arr' t a, Int' b)     -> Just $ Arr' (Tarr t) $ map (Arr' t) (replicate (fromInteger b) a)
          --       (Tup' a, Int' b)       -> Just $ Tup' $ map (Tup') (replicate (fromInteger b) a)
          --       (Arr' t1 a, Arr' t2 b) ->
          --         case sequence $ map (\i -> dvdv i (Arr' t2 b)) a of
          --           Just arr@(h:_) -> Just $ Arr' (val2typ h) arr 
          --           Just []        -> Just $ Arr' t1 []
          --           Nothing        -> Nothing
          --       (Arr' t a, b)          ->
          --         case sequence $ map (dvdv b) a of
          --           Nothing   -> Nothing
          --           Just vals ->
          --             case headMaybe vals of
          --               Just front -> Just $ Arr' (val2typ front) vals
          --               Nothing    -> Just $ Arr' t vals
          --       (a, Arr' t b)          ->
          --         case sequence $ map (dvdv a) b of
          --           Nothing   -> Nothing
          --           Just vals ->
          --             case headMaybe vals of
          --               Just front -> Just $ Arr' (val2typ front) vals
          --               Nothing    -> Just $ Arr' t vals
          --       (Tup' a, Tup' b)       ->
          --         case sequence $ map (\i -> dvdv i (Tup' b)) a of
          --           Nothing  -> Nothing
          --           Just tup -> Just $ Tup' tup 
          --       (Tup' a, b)            ->
          --         case sequence $ map (dvdv b) a of
          --           Nothing  -> Nothing
          --           Just tup -> Just $ Tup' tup
          --       (a, Tup' b)            ->
          --         case sequence $ map (dvdv a) b of
          --           Nothing  -> Nothing
          --           Just tup -> Just $ Tup' tup
                _                      -> Nothing
          in
            case dvdv l r of
              Nothing -> return Nothing
              Just result -> return $ Just (mr, result)

-- Might want to case about lhs later
cd :: Type -> Memory -> Expression -> Expression -> IO (Maybe (Memory, Value))
cd _ mem _ rhs = do
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
      \_ -> return $ Just (mem, Tup' [])

-- setenv :: Memory -> Expression -> Expression -> IO (Maybe (Memory, Value))
-- setenv mem _ rhs = do
--   right  <- interpret (Ttup $ tCollapse $ fromMaybe [Tany] $ exp2typs mem rhs) mem rhs
--   something
--   case right of
--     Just (mem', r) -> return $ Just (mem', Tup' [])

-- -- This evaluates the left side, and feeds it to the right's readProcessWithExitCode.
-- direct :: Type -> Memory -> Expression -> Expression -> IO (Maybe (Memory, Value))
-- direct = undefined

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
    handler :: IOError -> IO (Maybe String)
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
              putStr output
              exitCode <- waitForProcess procHand
              if exitCode /= ExitSuccess
              then putStrLn $ "[Process " ++ c ++ " exited with exit code: " ++ show exitCode ++ "]"
              else return ()
              -- Bash does this too. Might as well, since I don't want my args to contain newlines.
              return $ Just (filter (/= '\n') output)
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

-- Wanna make it better, but I don't know how.
argIO :: [Value] -> IO [String]
argIO vals =
  (
    sequence $ map
    (
      \v ->
        case v of
          Tup' _   -> do
            out <- cap v
            case out of
              Nothing -> return []
              Just txt -> return (words txt)
          Arr' _ _ -> do
            out <- cap v
            case out of
              Nothing -> return []
              Just txt -> return (words txt)
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
  -- (\outs -> return $ catMaybes outs)
