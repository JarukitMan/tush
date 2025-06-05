import qualified System.Environment as Env
import Interpret.Token
import Interpret.Data
import Interpret.Evaluate
import Interpret.Operators
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
          -- DEBUG
          putStrLn $ "\ESC[1;32m[TOKENS]\ESC[0m\n" ++ (show tokens)
          putStrLn $ "\ESC[1;32m[PARTS]\ESC[0m\n"  ++ (show $ bunch tokens)
          putStrLn $ "\ESC[1;32m[SENTENCE]\ESC[0m\n"  ++ ((show . parse) tokens)
          _ <- ((interpret Tany initmem) . parse) tokens
          return ()
        Left  errmsg -> putStrLn $ "\ESC[1;31m[ERROR]\ESC[0m\n" ++ errmsg
    Nothing -> do
      _ <- mainLoop initmem
      return ()

-- The main loop acts like a for loop here.
mainLoop :: Memory -> IO Memory
mainLoop mem = do
  cwd  <- getCurrentDirectory
  prompt <- tempprompt
  putStr prompt
  hFlush stdout
  line <- getLine
  if not $ "exit" == line then do
    putStrLn $ "\ESC[1;33m[DIR]\ESC[0m\n" ++ cwd
    case tokenize initmem line of
      Right tokens -> do
        -- DEBUG
        putStrLn $ "\ESC[1;32m[TOKENS]\ESC[0m\n" ++ (show tokens)
        putStrLn $ "\ESC[1;32m[PARTS]\ESC[0m\n"  ++ (show $ bunch tokens)
        putStrLn $ "\ESC[1;32m[SENTENCE]\ESC[0m\n"  ++ ((show . parse) tokens)
        result <- ((interpret Tany mem) . parse) tokens
        case result of
          Just (newmem, out) -> do
            -- output <- cap out
            -- case output of
            --   Nothing -> putStrLn "Nothing!"
            --   Just o -> putStr $ "\ESC[0m[CAPTURED]\n" ++ o ++ "\ESC[0m"
            cmd out
            putStrLn $ show out
            mainLoop newmem
          Nothing -> mainLoop mem
      Left  errmsg -> do
        putStrLn $ "\ESC[1;31m[ERROR]\ESC[0m\n" ++ errmsg
        mainLoop mem
  else
    return mem

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
        (",", Op 0 (empty, Nothing)),
        (".", Op 2 (empty, Nothing)),
        ("+", Op 3 (empty, Nothing)),
        ("-", Op 3 (empty, Nothing)),
        ("*", Op 4 (empty, Nothing)),
        ("/", Op 4 (empty, Nothing)),
        ("return", Op 1 (empty, Nothing))
      ]
  ]
