{-# LANGUAGE OverloadedStrings #-}
import qualified System.Environment as Env
import Tssl.Token
import Tssl.Data
import Tssl.Evaluate
import Tssl.Operators
import Misc
import Data.Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Tssl.Parse
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
      content <- T.readFile filename
      -- putStrLn $ "\ESC[1;36m[CONTENT]\ESC[0m\n" ++ content
      -- putStrLn $ unlines (map (\x -> case tokenize x of Left err -> err ; Right tokens -> show tokens) (lines content))
      case tokenize initmem content of
        Right tokens -> do
          -- DEBUG
          -- putStrLn $ "\ESC[1;32m[TOKENS]\ESC[0m\n" ++ (show tokens)
          -- putStrLn $ "\ESC[1;32m[PARTS]\ESC[0m\n"  ++ (show $ bunch initmem tokens)
          -- putStrLn $ "\ESC[1;32m[SENTENCE]\ESC[0m\n"  ++ ((show . parse initmem) tokens)
          result <- ((interpret True Tany initmem) . parse initmem) tokens
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
              -- if emptish out
              -- then return ()
              -- else T.putStrLn $ T.show out
              -- case out of
              --   Tup' [] -> return ()
              --   _ -> T.putStrLn $ T.show out
              case (T.intercalate " " . argify) out of
                "" -> return ()
                output -> T.putStrLn output
          return ()
        Left  errmsg -> T.putStrLn $ "\ESC[1;31m[ERROR]\ESC[0m\n" `T.append` errmsg
    Nothing -> do
      _ <- mainLoop True initmem
      return ()

-- The main loop acts like a for loop here.
mainLoop :: Bool -> Memory -> IO (Bool, Memory)
mainLoop gbs mem = do
  -- cwd  <- getCurrentDirectory
  prompt <- tempprompt
  T.putStr prompt
  hFlush stdout
  line <- T.getLine
  if not $ "exit" == line then do
    -- putStrLn $ "\ESC[1;33m[DIR]\ESC[0m\n" ++ cwd
    -- case tokenize mem ('\n':line) of
    case tokenize initmem line of
      Right tokens -> do
        -- DEBUG
        -- putStrLn $ "\ESC[1;32m[TOKENS]\ESC[0m\n" ++ (show tokens)
        -- putStrLn $ "\ESC[1;32m[PARTS]\ESC[0m\n"  ++ (show $ bunch mem tokens)
        -- putStrLn $ "\ESC[1;32m[SENTENCE]\ESC[0m\n"  ++ ((show . parse mem) tokens)
        let
          checkendline x =
            case x of
              Operand _          -> Expression 0 "," (Operand $ Tup []) x
              Expression _ _ _ _ ->
                -- if o `elem` ["|", "&", "$", ",", "cmd"]
                -- then x
                -- else Expression 0 "," (Operand $ Tup []) x
                Expression 0 "," (Operand $ Tup []) x
        result <- ((interpret gbs Tany mem) . (checkendline) . parse mem) tokens
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
            -- if emptish out
            -- then return ()
            -- else T.putStrLn $ T.show out
            case (T.intercalate " " . argify) out of
              "" -> return ()
              output -> T.putStrLn output
            -- case out of
            --   Tup' [] -> return ()
            --   -- Makeshift solution for the leftover () after all the operations.
            --   _ -> putStrLn $ show out
            mainLoop nbs newmem
          Nothing -> mainLoop gbs mem
      Left  errmsg -> do
        T.putStrLn $ "\ESC[1;31m[ERROR]\ESC[0m\n" `T.append` errmsg
        mainLoop gbs mem
  else
    return $ (gbs, mem)

-- The actual thing will fetch from the config file.
-- This is a placeholder function that doesn't actually exist.
tempprompt :: IO T.Text
tempprompt = do
  cwd <- getCurrentDirectory
  case headMaybe (reverse $ pieces (== '/') (T.pack cwd)) of
    Nothing -> return $ T.pack "\ESC[0m<|'w'|\ESC[33m*\ESC[0m> "
    Just path ->
      if T.null path
      then
        return $ T.pack "\ESC[0m<|'w'|\ESC[33m*\ESC[0m> "
      else
        return $ T.pack $ "\ESC[0m<|" ++ (T.unpack path) ++ "|\ESC[33m*\ESC[0m> "

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
            insertOp (Tany, Tany) (Base also Tany)
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
            insertOp (Tany, Tany) (Base wrt (Ttup []))
            (empty, Nothing)
          )
        ),
        ("=>", Op 2
          (
            insertOp (Tany, Tany) (Base apn (Ttup []))
            (empty, Nothing)
          )
        ),
        ("<-", Op 3
          (
            insertOp (Tany, Tany) (Base here Tstr)
            (empty, Nothing)
          )
        ),
        -- They can be of the same precedence, it's fine. The = always comes after anyways.
        ("let", Op 4
          (
            insertOp (Tany, Tany) (Base var (Ttup []))
            (empty, Nothing)
          )
        ),
        ("opr", Op 4
          (
            insertOp (Tany, Tany) (Base opr (Ttup []))
            (empty, Nothing)
          )
        ),
        ("set", Op 4
          (
            insertOp (Tany, Tany) (Base set (Ttup []))
            (empty, Nothing)
          )
        ),
        ("get", Op 4
          (
            insertOp (Ttup [], Tstr) (Base get (Tstr))
            (empty, Nothing)
          )
        ),
        ("unset", Op 4
          (
            insertOp (Tany, Tany) (Base unset (Ttup []))
            (empty, Nothing)
          )
        ),
        ("=", Op 4
          (
            insertOp (Tany, Tany) (Base asn (Ttup []))
            (empty, Nothing)
          )
        ),
        ("cd", Op 5
          (
            insertOp (Ttup [], Tany) (Base cd (Ttup []))
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
            insertOp (Tany, Tany) (Base ift Tany)
            (empty, Nothing)
          )
        ),
        ("for", Op 6
          (
            insertOp (Tint, Tany) (Base range (Tarr Tint)) $
            insertOp (Tflt, Tany) (Base range (Tarr Tflt)) $
            insertOp (Tchr, Tany) (Base range (Tarr Tchr)) $
            insertOp (Tstr, Tany) (Base range (Tarr Tstr)) $
            insertOp (Tpth, Tany) (Base range (Tarr Tpth)) $
            insertOp (Tbln, Tany) (Base range (Tarr Tbln)) $
            insertOp (Tany, Tany) (Base frl Tany)
            (empty, Nothing)
          )
        ),
        ("foreach", Op 6
          (
            insertOp (Tint, Tany) (Base range (Tarr Tint)) $
            insertOp (Tflt, Tany) (Base range (Tarr Tflt)) $
            insertOp (Tchr, Tany) (Base range (Tarr Tchr)) $
            insertOp (Tstr, Tany) (Base range (Tarr Tstr)) $
            insertOp (Tpth, Tany) (Base range (Tarr Tpth)) $
            insertOp (Tbln, Tany) (Base range (Tarr Tbln)) $
            insertOp (Tany, Tany) (Base fre Tany)
            (empty, Nothing)
          )
        ),
        ("while", Op 6
          (
            insertOp (Tint, Tany) (Base range (Tarr Tint)) $
            insertOp (Tflt, Tany) (Base range (Tarr Tflt)) $
            insertOp (Tchr, Tany) (Base range (Tarr Tchr)) $
            insertOp (Tstr, Tany) (Base range (Tarr Tstr)) $
            insertOp (Tpth, Tany) (Base range (Tarr Tpth)) $
            insertOp (Tbln, Tany) (Base range (Tarr Tbln)) $
            insertOp (Tany, Tany) (Base whl Tany)
            (empty, Nothing)
          )
        ),
        ("else", Op 6
          (
            insertOp (Tany, Tany) (Base els Tany)
            (empty, Nothing)
          )
        ),
        ("return", Op 7
          (
            insertOp (Ttup [], Tany) (Base ret (Tany))
            (empty, Nothing)
          )
        ),
        ("break", Op 7
          (
            insertOp (Ttup [], Ttup []) (Base brk (Tbrk))
            (empty, Nothing)
          )
        ),
        -- ("continue", Op 7
        --   (
        --     (empty, Nothing)
        --   )
        -- ),
        ("&&", Op 8
          (
            insertOp (Tbln, Tbln) (Base Tssl.Operators.and (Tbln))
            (empty, Nothing)
          )
        ),
        ("||", Op 8
          (
            insertOp (Tbln, Tbln) (Base Tssl.Operators.or (Tbln))
            (empty, Nothing)
          )
        ),
        ("not", Op 9
          (
            insertOp (Ttup [], Tbln) (Base opposite (Tbln))
            (empty, Nothing)
          )
        ),
        ("==", Op 10
          (
            insertOp (Tany, Tany) (Base equal (Tbln))
            (empty, Nothing)
          )
        ),
        ("/=", Op 10
          (
            insertOp (Tany, Tany) (Base unequal (Tbln))
            (empty, Nothing)
          )
        ),
        (">=", Op 10
          (
            insertOp (Tany, Tany) (Base moe (Tbln))
            (empty, Nothing)
          )
        ),
        ("<=", Op 10
          (
            insertOp (Tany, Tany) (Base loe (Tbln))
            (empty, Nothing)
          )
        ),
        ("<", Op 10
          (
            insertOp (Tany, Tany) (Base less (Tbln))
            (empty, Nothing)
          )
        ),
        (">", Op 10
          (
            insertOp (Tany, Tany) (Base more (Tbln))
            (empty, Nothing)
          )
        ),
        ("..", Op 11
          (
            insertOp (Tint, Tint) (Base range (Tarr Tint)) $
            insertOp (Tint, Tflt) (Base range (Tarr Tflt)) $
            insertOp (Tflt, Tint) (Base range (Tarr Tflt)) $
            insertOp (Tflt, Tflt) (Base range (Tarr Tflt)) $
            insertOp (Ttup [Tint, Tint], Tint) (Base range (Tarr Tflt)) $
            insertOp (Ttup [Tflt, Tint], Tint) (Base range (Tarr Tflt)) $
            insertOp (Ttup [Tint, Tflt], Tint) (Base range (Tarr Tflt)) $
            insertOp (Ttup [Tflt, Tflt], Tint) (Base range (Tarr Tflt)) $
            insertOp (Ttup [Tint, Tint], Tflt) (Base range (Tarr Tflt)) $
            insertOp (Ttup [Tflt, Tint], Tflt) (Base range (Tarr Tflt)) $
            insertOp (Ttup [Tint, Tflt], Tflt) (Base range (Tarr Tflt)) $
            insertOp (Ttup [Tflt, Tflt], Tflt) (Base range (Tarr Tflt)) $
            insertOp (Tchr, Tchr) (Base range (Tarr Tchr)) $
            insertOp (Ttup [Tchr, Tchr], Tchr) (Base range (Tarr Tchr)) $
            insertOp (Tpth, Tpth) (Base range (Tarr Tpth)) $
            insertOp (Ttup [], Tpth) (Base range (Tpth)) $
            insertOp (Ttup [], Ttup []) (Base range (Tpth))
            (empty, Nothing)
          )
        ),
        ("+", Op 12
          (
            insertOp (Tint, Tint) (Base add (Tint)) $
            insertOp (Tint, Tflt) (Base add (Tflt)) $
            insertOp (Tflt, Tint) (Base add (Tflt)) $
            insertOp (Tflt, Tflt) (Base add (Tflt)) $
            insertOp (Tint, Tchr) (Base add (Tint)) $
            insertOp (Tchr, Tint) (Base add (Tchr)) $
            insertOp (Tchr, Tchr) (Base add (Tchr)) $
            insertOp (Tbln, Tbln) (Base add (Tbln)) $
            insertOp (Tpth, Tstr) (Base add (Tpth)) $
            -- insertOp (Tstr, Tany) (Base add (Tstr)) $
            -- insertOp (Tany, Tstr) (Base add (Tstr)) $
            -- insertOp (Tarr Tany, Tarr Tany) (Base add (Tarr Tany)) $
            -- insertOp (Tarr Tany, Tany) (Base add (Tarr Tany)) $
            insertOp (Tpth, Tint) (Base add (Tpth)) $
            insertOp (Tany, Tany) (Base add (Tany))
            (empty, Nothing)
          )
        ),
        ("-", Op 12
          (
            insertOp (Tint, Tint) (Base sub (Tint)) $
            insertOp (Tint, Tflt) (Base sub (Tflt)) $
            insertOp (Ttup [], Tint) (Base sub (Tint)) $
            insertOp (Ttup [], Tflt) (Base sub (Tflt)) $
            insertOp (Tflt, Tint) (Base sub (Tflt)) $
            insertOp (Tflt, Tflt) (Base sub (Tflt)) $
            insertOp (Tint, Tchr) (Base sub (Tint)) $
            insertOp (Tchr, Tint) (Base sub (Tchr)) $
            insertOp (Tchr, Tchr) (Base sub (Tchr)) $
            insertOp (Tstr, Tint) (Base sub (Tstr)) $
            insertOp (Tarr Tany, Tint) (Base sub (Tarr Tany)) $
            insertOp (Tstr, Tchr) (Base sub (Tstr)) $
            insertOp (Tstr, Tstr) (Base sub (Tstr)) $
            insertOp (Tarr Tany, Tarr Tany) (Base sub (Tarr Tany)) $
            insertOp (Tpth, Tint) (Base sub (Tpth)) $
            insertOp (Tany, Tany) (Base sub (Tany))
            (empty, Nothing)
          )
        ),
        ("*", Op 13
          (
            insertOp (Tint, Tint) (Base mpy (Tint)) $
            insertOp (Tint, Tflt) (Base mpy (Tflt)) $
            insertOp (Tflt, Tint) (Base mpy (Tflt)) $
            insertOp (Tflt, Tflt) (Base mpy (Tflt)) $
            insertOp (Tint, Tchr) (Base mpy (Tint)) $
            insertOp (Tchr, Tint) (Base mpy (Tstr)) $
            insertOp (Tbln, Tbln) (Base mpy (Tbln)) $
            insertOp (Tstr, Tint) (Base mpy (Tstr)) $
            insertOp (Tarr Tany, Tint) (Base mpy (Tarr Tany)) $
            insertOp (Tarr Tany, Tany) (Base mpy (Tarr Tany)) $
            insertOp (Tany, Tarr Tany) (Base mpy (Tarr Tany)) $
            insertOp (Tarr Tany, Tarr Tany) (Base mpy (Tarr Tany)) $
            insertOp (Tany, Tany) (Base mpy (Tany))
            (empty, Nothing)
          )
        ),
        ("/", Op 13
          (
            insertOp (Tint, Tint) (Base dvd (Tint)) $
            insertOp (Tint, Tflt) (Base dvd (Tflt)) $
            insertOp (Tflt, Tint) (Base dvd (Tflt)) $
            insertOp (Tflt, Tflt) (Base dvd (Tflt)) $
            insertOp (Tstr, Tint) (Base dvd (Tarr Tstr)) $
            insertOp (Tarr Tany, Tint) (Base dvd (Tarr $ Tarr Tany)) $
            insertOp (Tarr Tany, Tany) (Base dvd (Tarr Tany)) $
            insertOp (Tany, Tarr Tany) (Base dvd (Tarr Tany)) $
            insertOp (Tany, Tany) (Base dvd (Tany))
            (empty, Nothing)
          )
        ),
        ("len", Op 14
          (
            insertOp (Ttup [], Tany) (Base len (Tint))
            (empty, Nothing)
          )
        ),
        ("int", Op 15
          (
            insertOp (Ttup [], Tint) (Base int (Tint)) $
            insertOp (Ttup [], Tflt) (Base int (Tint)) $
            insertOp (Ttup [], Tbln) (Base int (Tint)) $
            insertOp (Ttup [], Tchr) (Base int (Tint)) $
            insertOp (Ttup [], Tstr) (Base int (Tint))
            -- insertOp (Ttup [], Tany) (Base int (Tint))
            (empty, Nothing)
          )
        ),
        ("flt", Op 15
          (
            insertOp (Ttup [], Tint) (Base flt (Tflt)) $
            insertOp (Ttup [], Tflt) (Base flt (Tflt)) $
            insertOp (Ttup [], Tbln) (Base flt (Tflt)) $
            insertOp (Ttup [], Tchr) (Base flt (Tflt)) $
            insertOp (Ttup [], Tstr) (Base flt (Tflt))
            -- insertOp (Ttup [], Tany) (Base flt (Tflt))
            (empty, Nothing)
          )
        ),
        ("chr", Op 15
          (
            insertOp (Ttup [], Tint) (Base chr (Tchr)) $
            insertOp (Ttup [], Tflt) (Base chr (Tchr)) $
            insertOp (Ttup [], Tbln) (Base chr (Tchr)) $
            insertOp (Ttup [], Tchr) (Base chr (Tchr)) $
            insertOp (Ttup [], Tstr) (Base chr (Tchr))
            -- insertOp (Ttup [], Tany) (Base chr (Tchr))
            (empty, Nothing)
          )
        ),
        ("bln", Op 15
          (
            insertOp (Ttup [], Tint) (Base bln (Tbln)) $
            insertOp (Ttup [], Tflt) (Base bln (Tbln)) $
            insertOp (Ttup [], Tbln) (Base bln (Tbln)) $
            insertOp (Ttup [], Tchr) (Base bln (Tbln)) $
            insertOp (Ttup [], Tstr) (Base bln (Tbln))
            -- insertOp (Ttup [], Tany) (Base bln (Tbln))
            (empty, Nothing)
          )
        ),
        ("typ", Op 15
          (
            insertOp (Ttup [], Tany) (Base typ (Ttyp))
            (empty, Nothing)
          )
        ),
        ("str", Op 15
          (
            insertOp (Ttup [], Tany) (Base str (Tstr))
            (empty, Nothing)
          )
        ),
        ("pth", Op 15
          (
            insertOp (Ttup [], Tstr) (Base pth (Tpth)) $
            insertOp (Ttup [], Tpth) (Base pth (Tpth)) $
            insertOp (Ttup [], Tarr Tstr) (Base pth (Tpth))
            (empty, Nothing)
          )
        ),
        (":", Op 16
          (
            insertOp (Tstr, Tstr) (Base acs Tpth) $
            insertOp (Tany, Tarr Tint) (Base acs Tany) $
            insertOp (Tany, Tint) (Base acs Tany) $
            insertOp (Tstr, Tint) (Base acs Tchr) $
            insertOp (Tstr, Tarr Tint) (Base acs Tstr) $
            insertOp (Tpth, Tint) (Base acs Tpth)
            (empty, Nothing)
          )
        ),
        ("!!", Op 16
          (
            insertOp (Tstr, Tstr) (Base acs Tpth) $
            insertOp (Tany, Tarr Tint) (Base acs Tany) $
            insertOp (Tany, Tint) (Base acs Tany) $
            insertOp (Tstr, Tint) (Base acs Tchr) $
            insertOp (Tstr, Tarr Tint) (Base acs Tstr) $
            insertOp (Tpth, Tstr) (Base acs Tpth)
            (empty, Nothing)
          )
        ),
        -- Might bump to 255 later.
        ("$", Op 254
          (
            insertOp (Tarr $ Ttup [Tstr, Tstr], Tany) (Base exe $ Tarr Tstr) $
            insertOp (Ttup [], Tany) (Base exe $ Tarr Tstr)
            (empty, Nothing)
          )
        ),
        ("cmd", Op 254
          (
            insertOp (Tarr $ Ttup [Tstr, Tstr], Tany) (Base exe $ Tarr Tstr) $
            insertOp (Ttup [], Tany) (Base exe $ Tarr Tstr)
            (empty, Nothing)
          )
        ),
        ("with", Op 254
          (
            insertOp (Tany, Tarr $ Ttup [Tstr, Tstr]) (Base with $ Ttup [])
            (empty, Nothing)
          )
        ),
        ("input", Op 255
          (
            insertOp (Ttup [], Ttup []) (Base input Tstr)
            (empty, Nothing)
          )
        )
      ]
  ]
