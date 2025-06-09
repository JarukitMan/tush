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
      -- putStrLn $ "\ESC[1;34m[ARGS]\ESC[0m\n" ++ (show args)
      content <- readFile filename
      -- putStrLn $ "\ESC[1;36m[CONTENT]\ESC[0m\n" ++ content
      -- putStrLn $ unlines (map (\x -> case tokenize x of Left err -> err ; Right tokens -> show tokens) (lines content))
      case tokenize initmem content of
        Right tokens -> do
          -- DEBUG
          -- putStrLn $ "\ESC[1;32m[TOKENS]\ESC[0m\n" ++ (show tokens)
          -- putStrLn $ "\ESC[1;32m[PARTS]\ESC[0m\n"  ++ (show $ bunch tokens)
          -- putStrLn $ "\ESC[1;32m[SENTENCE]\ESC[0m\n"  ++ ((show . parse) tokens)
          result <- ((interpret True Tany initmem) . parse) tokens
          case result of
            Nothing -> return ()
            Just (_, _, out) -> do
              -- output <- cap out
              -- case output of
              --   Nothing -> putStrLn "Nothing!"
              --   Just o -> putStr $ "\ESC[0m[CAPTURED]\n" ++ o ++ "\ESC[0m"
              -- DEBUG: The real thing won't be using cmd since it'll be baked into
              -- the evaluation function. (the "," operation.)
              -- This means the real thing needs to get newlines too.
              -- cmd out
              case out of
                Tup' [] -> return ()
                _ -> putStrLn $ show out
          return ()
        Left  errmsg -> putStrLn $ "\ESC[1;31m[ERROR]\ESC[0m\n" ++ errmsg
    Nothing -> do
      _ <- mainLoop True initmem
      return ()

-- The main loop acts like a for loop here.
mainLoop :: Bool -> Memory -> IO (Bool, Memory)
mainLoop gbs mem = do
  -- cwd  <- getCurrentDirectory
  prompt <- tempprompt
  putStr prompt
  hFlush stdout
  line <- getLine
  if not $ "exit" == line then do
    -- putStrLn $ "\ESC[1;33m[DIR]\ESC[0m\n" ++ cwd
    case tokenize initmem ('\n':line) of
    -- case tokenize initmem line of
      Right tokens -> do
        -- DEBUG
        -- putStrLn $ "\ESC[1;32m[TOKENS]\ESC[0m\n" ++ (show tokens)
        -- putStrLn $ "\ESC[1;32m[PARTS]\ESC[0m\n"  ++ (show $ bunch tokens)
        -- putStrLn $ "\ESC[1;32m[SENTENCE]\ESC[0m\n"  ++ ((show . parse) tokens)
        result <- ((interpret gbs Tany mem) . parse) tokens
        case result of
          Just (nbs, newmem, out) -> do
            -- output <- cap out
            -- case output of
            --   Nothing -> putStrLn "Nothing!"
            --   Just o -> putStr $ "\ESC[0m[CAPTURED]\n" ++ o ++ "\ESC[0m"
            -- DEBUG: The real thing won't be using cmd since it'll be baked into
            -- the evaluation function. (the "," operation.)
            -- This means the real thing needs to get newlines too.
            -- cmd out
            case out of
              Tup' [ ] -> return ()
              -- Makeshift solution for the leftover () after all the operations.
              _ -> putStrLn $ show out
            mainLoop nbs newmem
          Nothing -> mainLoop gbs mem
      Left  errmsg -> do
        putStrLn $ "\ESC[1;31m[ERROR]\ESC[0m\n" ++ errmsg
        mainLoop gbs mem
  else
    return $ (gbs, mem)

-- The actual thing will fetch from the config file.
-- This is a placeholder function that doesn't actually exist.
tempprompt :: IO String
tempprompt = do
  cwd <- getCurrentDirectory
  case headMaybe (reverse $ pieces (== '/') cwd) of
    Nothing -> return $ '<':'|':("'w'|\ESC[33m*\ESC[0m> ")
    Just "" -> return $ '<':'|':("'w'|\ESC[33m*\ESC[0m> ")
    Just path -> return $ '<':'|':(path ++ "|\ESC[33m*\ESC[0m> ")

initmem :: Memory
initmem =
  [
    fromList
      [
        (",", Op 0
          (
            insertOp (Tany, Tany) (Base next Tany)
            (empty, Nothing)
          )
        ),
        ("&", Op 0 (
            (empty, Nothing)
          )
        ),
        ("|", Op 1
          (
            insertOp (Tany, Tany) (Base pipe Tstr)
            (empty, Nothing)
          )
        ),
        ("->", Op 2
          (
            (empty, Nothing)
          )
        ),
        ("=>", Op 2
          (
            (empty, Nothing)
          )
        ),
        ("<-", Op 3
          (
            (empty, Nothing)
          )
        ),
        ("let", Op 4
          (
            (empty, Nothing)
          )
        ),
        ("opr", Op 4
          (
            (empty, Nothing)
          )
        ),
        ("=", Op 4
          (
            (empty, Nothing)
          )
        ),
        ("cd", Op 5
          (
           insertOp (Tany, Tany) (Base cd (Ttup []))
            (empty, Nothing)
          )
        ),
        ("help", Op 5
          (
            (empty, Nothing)
          )
        ),
        ("time", Op 5
          (
            (empty, Nothing)
          )
        ),
        ("if", Op 6
          (
            (empty, Nothing)
          )
        ),
        ("for", Op 6
          (
            (empty, Nothing)
          )
        ),
        ("while", Op 6
          (
            (empty, Nothing)
          )
        ),
        ("else", Op 6
          (
            (empty, Nothing)
          )
        ),
        -- ("return", Op 7
        --   (
        --     (empty, Nothing)
        --   )
        -- ),
        ("break", Op 7
          (
            (empty, Nothing)
          )
        ),
        ("continue", Op 7
          (
            (empty, Nothing)
          )
        ),
        ("&&", Op 8
          (
            (empty, Nothing)
          )
        ),
        ("||", Op 8
          (
            (empty, Nothing)
          )
        ),
        ("not", Op 9
          (
            (empty, Nothing)
          )
        ),
        ("==", Op 10
          (
            (empty, Nothing)
          )
        ),
        ("/=", Op 10
          (
            (empty, Nothing)
          )
        ),
        (">=", Op 10
          (
            (empty, Nothing)
          )
        ),
        ("<=", Op 10
          (
            (empty, Nothing)
          )
        ),
        ("<", Op 10
          (
            (empty, Nothing)
          )
        ),
        (">", Op 10
          (
            (empty, Nothing)
          )
        ),
        ("..", Op 11
          (
            (empty, Nothing)
          )
        ),
        ("+", Op 12
          (
            (empty, Nothing)
          )
        ),
        ("-", Op 12
          (
            (empty, Nothing)
          )
        ),
        ("*", Op 13
          (
            (empty, Nothing)
          )
        ),
        ("/", Op 13
          (
            (empty, Nothing)
          )
        ),
        ("len", Op 14
          (
            (empty, Nothing)
          )
        ),
        ("int", Op 15
          (
            (empty, Nothing)
          )
        ),
        ("flt", Op 15
          (
            (empty, Nothing)
          )
        ),
        ("chr", Op 15
          (
            (empty, Nothing)
          )
        ),
        ("bln", Op 15
          (
            (empty, Nothing)
          )
        ),
        ("typ", Op 15
          (
            (empty, Nothing)
          )
        ),
        ("str", Op 15
          (
            (empty, Nothing)
          )
        ),
        ("pth", Op 15
          (
            (empty, Nothing)
          )
        ),
        (".", Op 16
          (
            insertOp (Tstr, Tstr) (Base dot Tstr) $
            insertOp (Tany, Tarr Tint) (Base dot Tany) $
            insertOp (Tany, Tint) (Base dot Tany) $
            insertOp (Tany, Tstr) (Base dot Tany)
            (empty, Nothing)
          )
        ),
        -- Might bump to 255 later.
        ("$", Op 254
          (
            (empty, Nothing)
          )
        ),
        ("cmd", Op 254
          (
            (empty, Nothing)
          )
        ),
        ("input", Op 255
          (
            (empty, Nothing)
          )
        )
      ]
  ]
