import qualified System.Environment as Env
import Token

-- Flags not implemented yet.
main = do
  args <- Env.getArgs
  if not (null args) then do
    putStrLn $ "Args: " ++ (show args)
    content <- readFile $ head args
    putStrLn $ "Content: " ++ content
    putStrLn $ unlines (map (\x -> case tokenize x of Left err -> err ; Right tokens -> show tokens) (lines content))
  else
    mainLoop

-- The main loop acts like a for loop here.
mainLoop = do
  line <- getLine
  if not $ null line then do
    case tokenize line of
      Left err -> putStrLn err
      Right tokens -> putStrLn $ (unlines . map (show)) tokens
    mainLoop
  else
    return ()
