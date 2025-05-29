import qualified System.Environment as Env
import Interpret.Token
import Interpret.Data
import Interpret.Evaluate
import Misc
import Data.Map
import Interpret.Parse
import System.Directory
import System.IO

-- Flags not implemented yet.
main :: IO ()
main = do
  args <- Env.getArgs
  let arg1 = headMaybe args
  case arg1 of
    Just filename -> do
      putStrLn $ "\ESC[1;34m[ARGS]\ESC[0m\n" ++ (show args)
      content <- readFile filename
      putStrLn $ "\ESC[1;36m[CONTENT]\ESC[0m\n" ++ content
      -- putStrLn $ unlines (map (\x -> case tokenize x of Left err -> err ; Right tokens -> show tokens) (lines content))
      case tokenize initmem content of
        Right tokens -> do
          putStrLn $ "\ESC[1;32m[TOKENS]\ESC[0m\n" ++ (show tokens)
          putStrLn $ "\ESC[1;32m[PARTS]\ESC[0m\n"  ++ (show $ bunch tokens)
          putStrLn $ "\ESC[1;32m[SENTENCE]\ESC[0m\n"  ++ ((show . parse) tokens)
        Left  errmsg -> putStrLn $ "\ESC[1;31m[ERROR]\ESC[0m\n" ++ errmsg
    Nothing -> mainLoop

-- The main loop acts like a for loop here.
mainLoop :: IO ()
mainLoop = do
  cwd  <- getCurrentDirectory
  prompt <- tempprompt
  putStr prompt
  hFlush stdout
  line <- getLine
  if not $ Prelude.null line then do
    putStrLn $ "\ESC[1;33m[DIR]\ESC[0m\n" ++ cwd
    case tokenize initmem line of
      Right tokens -> do
        putStrLn $ "\ESC[1;32m[TOKENS]\ESC[0m\n" ++ (show tokens)
        putStrLn $ "\ESC[1;32m[PARTS]\ESC[0m\n"  ++ (show $ bunch tokens)
        putStrLn $ "\ESC[1;32m[SENTENCE]\ESC[0m\n"  ++ ((show . parse) tokens)
        _ <- ((interpret initmem) . treeify (Operand $ Tup []) . bunch) tokens
        return ()
      Left  errmsg -> putStrLn $ "\ESC[1;31m[ERROR]\ESC[0m\n" ++ errmsg
    mainLoop
  else
    return ()

-- The actual thing will fetch from the config file.
-- This is a placeholder function that doesn't actually exist.
tempprompt :: IO String
tempprompt = do
  cwd <- getCurrentDirectory
  case (headMaybe . reverse . (pieces (== '/'))) cwd of
    Just path -> return $ '<':'|':path ++ "|*> "
    Nothing   -> return "<|'w'|*>"

initmem :: Memory
initmem =
  [
    fromList
    [
      (
        ",", Op 0
        (
          fromList
          [
            (
              ([Tany], [Tany]), Base (\m l r -> do {putStrLn $ show l ++ show r ; return $ Just (m, None)}) Tany
            )
          ]
        )
      )
    ]
  ]
