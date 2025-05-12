import qualified System.Environment as Env
import Token
import Misc

-- Flags not implemented yet.
main :: IO ()
main = do
  args <- Env.getArgs
  let arg1 = headMaybe args
  case arg1 of
    Just filename -> do
      putStrLn $ "Args: " ++ (show args)
      content <- readFile filename
      putStrLn $ "Content: " ++ content
      -- putStrLn $ unlines (map (\x -> case tokenize x of Left err -> err ; Right tokens -> show tokens) (lines content))
      putStrLn $ "Tokens: " ++ show (tokenize content)
    Nothing -> mainLoop

-- The main loop acts like a for loop here.
mainLoop :: IO ()
mainLoop = do
  line <- getLine
  if not $ null line then do
    case tokenize line of
      Left err -> putStrLn err
      Right tokens -> putStrLn $ show tokens
    mainLoop
  else
    return ()
