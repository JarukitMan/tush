{-# LANGUAGE OverloadedStrings #-}
module Grr.Regex(regex) where
import qualified Data.Text as T
import Misc
import Grr.NamedRanges
import qualified Data.Set as S
import GHC.Utils.Misc
import Data.List((!?))

data Anchor = BeginLine | EndLine | BeginWord | EndWord | EdgeWord | NotEdgeWord
  deriving (Show)

data RegexPrimitive = WildCard | Char Char | Range Bool T.Text | Anchor Anchor | BackRef Int | Parens [Regex]
  deriving (Show)

data RegexRepetition = Between Int Int | Exact Int | AtLeast Int -- AtMost is just Between 0, n.
  deriving (Show)

-- See, the list is nested because of the or bars. If there's none, you just get a singleton.
type Regex = [(RegexPrimitive, RegexRepetition)]

-- First is the current index. The second is the backref starts and ends.
-- A no-match removes the entry from the set.
type Branches = S.Set (Int, [(Int, Int)])

data RegexSymbol =
  Star | Dot | Question | Plus | Bar | Times Int | Btw Int Int | Mrt Int |
  Paren [RegexSymbol] | Lit Char | Bracket Bool T.Text | Back Int | Anch Anchor
  deriving (Show)

-- Using Text for things like this is fine, because uncons is also O(1). (Just move the pointer one char back.)
regex :: T.Text -> T.Text -> Bool
regex pat txt =
  case txt2rgx pat of
    Nothing -> False
    Just p  -> match p txt

-- Lazy. For exacts, consume exact. For those in a range, map from consume small to big then ||.
-- Returns exact matches only.
match :: [Regex] -> T.Text -> Bool
match pat txt =
  or
  (
    map
    (
      \x ->
      T.length txt `elem`
      (
        S.map fst
        (
          internalMatch
          (S.singleton (0, []))
          x txt
        )
      )
    )
    pat
  )

-- Internal version that contains info for backreferences.
internalMatch :: Branches -> Regex -> T.Text -> Branches
internalMatch bs pat txt =
  case pat of
    (p:ps) -> internalMatch (matchOne bs p txt) ps txt
    -- Because the successful match would have ended the tracker right after the last character of the text.
    [] -> bs

matchOne :: Branches -> (RegexPrimitive, RegexRepetition) -> T.Text -> Branches
matchOne bs (p, r) txt =
  case r of
    Exact a -> nTimes a (\x -> matchOnce x p txt) bs
    -- This will probably be the same as above, but mapped over from a times to b times.
    -- Could be more efficient by using the branches from the previous match. Can convert it to fold.
    -- I could reuse it for AtLeast if I folded instead too.
    Between a b ->
      -- Sketchy.
      snd $ while
      (\(i, br) -> i > 0 && (not $ S.null $ matchOnce br p txt `S.difference` br))
      -- This merges the two iterations together. If not done, the previous branches would be replaced.
      -- The goal is to have all the branches between a and b, so that's unacceptable.
      (\(i, br) -> (pred i, S.union br $ matchOnce br p txt))
      -- This copies the AtLeast because it's between the two numbers, not a naive subtraction.
      (b - a, nTimes a (\x -> matchOnce x p txt) bs)
      -- S.unions (map (\n -> nTimes n (\x -> matchOnce x p txt) bs) [a..b])
    -- No idea about this though. Map until it returns an empty? Maybe to infinity?
    -- Well, I just looked and it seems strict. I guess I'll have to map it until it's empty.
    -- ...Or completely the same. You can't expect the user not to give you something that moves the cursor by 0.
    AtLeast a ->
      while
      (\br -> not $ S.null $ matchOnce br p txt `S.difference` br)
      (\br -> S.union br $ matchOnce br p txt) $
      nTimes a (\x -> matchOnce x p txt) bs
    where
      while :: (a -> Bool) -> (a -> a) -> a -> a
      while c f x = if c x then while c f (f x) else x

-- This function matches the pattern once. This differs from the function above in the fact that
-- In theory, this should handle all the branch pruning.
-- this function deals with the primitives, while the one above deals with the repetitions.
matchOnce :: Branches -> RegexPrimitive -> T.Text -> Branches
matchOnce bs p txt =
  case p of
    -- Sketchy.
      -- Check if the length is exceeded so I don't do redundant stuff... Equal to length is fine.
      -- The other ones already implicitly do it.
    WildCard -> let len = T.length txt in  check ((\i -> i < len) . fst)
    Char c -> check ((\i -> txt `tIndexMaybe` i == Just c) . fst)
    Range b cs -> check ((\i -> maybe False (\x -> x `T.elem` cs /= b) (txt `tIndexMaybe` i)) . fst)
    -- This one would depend on the anchor type. It's definitely checking the current pointer and the previous character, though.
    -- Note that this doesn't use check because it does not succ.
    -- So it's the current and the -1.
    Anchor anch ->
      S.filter
      (
        \(i, _) ->
          let
            tim n =
              case txt `tIndexMaybe` n of 
                Nothing -> '\n'
                Just x -> x
            prev = tim (i - 1)
            next = tim i
          in
            checkAnch (prev, next) anch
      )
      bs
    -- This also doesn't use check because it consumes the entire prefix.
    -- The texts need to be fetched from the memory...
    BackRef br ->
      S.map
      (
        \(i, r) ->
        case r !? (br - 1) of
          Just (a, b) -> (i + (b - a), r)
          -- Really, really shouldn't happen.
          Nothing -> (-1, r)
      ) $
      S.filter
      (
        \(i, r) ->
        case r !? (br - 1) of
          Nothing -> False
          -- Sketchy.
          Just (s, e) -> (T.drop s $ T.take e txt) `T.isPrefixOf` (T.drop i txt)
      )
      bs
    -- This also doesn't use check because it consumes an entire block of regex.
    -- Sketchy.
    Parens pats ->
      -- I have to find a way to carry over the old index to pair with the new indices.
      -- So I need to map it over the multiple patterns first. Then unions it back.
      -- The plan is... S.map (i, r) into Set of (i, r)s, right? Then I map the OLD i
      -- into the new rs alongside the new i. After all that, I unions the branches from the ors.
      S.unions $ map
      (
        \pat ->
        S.unions $ S.map
        (
          \b@(i, _) ->
          S.map
          (
            \(ni, nr) ->
            (ni, (i, ni):nr)
          )
          (internalMatch (S.singleton b) pat txt)
        )
        bs
      )
      pats
  where check f = S.map (\(i, r) -> (succ i, r)) $ S.filter f bs

-- This function checks if the "point between the previous character and the next character"
-- matches the Anchor provided or not. EG. ('a', ' ') matches EndWord. A BeginWord also matches EndWord.
-- I would convert the chars into anchors first, then match them.
checkAnch :: (Char, Char) -> Anchor -> Bool
checkAnch (previous, next) anchor =
  case anchor of
    BeginLine -> previous `T.elem` "\n\r"
    EndLine -> next `T.elem` "\n\r"
    BeginWord -> bw
    EndWord -> ew
    EdgeWord -> bw || ew
    NotEdgeWord -> not $ bw || ew
    where
      bw = previous `T.elem` space && (not $ next `T.elem` space)
      ew = (not $ previous `T.elem` space) && next `T.elem` space

-- Since it's multi-level, I'll have to do the classic Tokenize -> Parse
txt2rgx :: T.Text -> Maybe [Regex]
txt2rgx txt = tokenize txt >>= \tokens -> parse tokens

-- If no repetition symbol, put in Exact 1
-- If multiple repetitions in a row, return a Nothing.
parse :: [RegexSymbol] -> Maybe [Regex]
parse (x1:x2:xs) =
  case (sym2pmt x1, sym2rep x2) of
    (Just p, Just r) -> do
      rest <- parse xs
      case rest of
        (y:ys) -> return (((p, r):y):ys)
        [] -> return [[(p, r)]]
    -- This sends it to the next iteration. If it's a bar, it'd go to the branch under.
    -- If it's a primitive, then this would be recognized as a 1-match, and the parsing continues.
    (Just p, Nothing) -> do
      rest <- parse (x2:xs)
      case rest of
        (y:ys) -> return (((p, Exact 1):y):ys)
        [] -> return [[(p, Exact 1)]]
    (Nothing, _) ->
      case x1 of
        Bar -> do
          rest <- parse (x2:xs)
          return $ []:rest
        _ -> Nothing
parse [x] =
  case x of
    Bar ->  return [[]]
    _ -> do
      p <- sym2pmt x
      return [[(p, Exact 1)]]
parse [] = return []

sym2pmt :: RegexSymbol -> Maybe RegexPrimitive
sym2pmt x =
  case x of
    Dot -> return WildCard
    Paren symbols -> do
      inside <- parse symbols
      return $ Parens inside
    Lit c -> return $ Char c
    -- Sketchy
    Bracket b txt -> return $ Range b txt
    Back i -> return $ BackRef i
    Anch a -> return $ Anchor a
    _ -> Nothing

sym2rep :: RegexSymbol -> Maybe RegexRepetition
sym2rep x =
  case x of
    Star -> return $ AtLeast 0
    Question -> return $ Between 0 1
    Plus -> return $ AtLeast 1
    Times a -> return $ Exact a
    Btw a b -> return $ Between a b
    Mrt a -> return $ AtLeast a
    _ -> Nothing

tokenize :: T.Text -> Maybe [RegexSymbol]
tokenize (x T.:< txt)
  |
  x == '*' = do
    rest <- tokenize txt
    return $ Star:rest
  |
  x == '.' = do
    rest <- tokenize txt
    return $ Dot:rest
  |
  x == '?' = do
    rest <- tokenize txt
    return $ Question:rest
  |
  x == '+' = do
    rest <- tokenize txt
    return $ Plus:rest
  |
  x == '|' = do
    rest <- tokenize txt
    return $ Bar:rest
  |
  x == '{' =
    case splitInside 1 '{' txt of
      Nothing -> Nothing
      -- Nothing -> Left "Unclosed curly bracket '{'"
      Just (inside, outside) -> do
        rest <- tokenize outside
        if ',' `T.elem` inside
        then
          case T.span (/= ',') inside of
            (T.Empty, T.Empty) -> Nothing
            (front, ",") -> do
              a <- intMaybe front
              return $ Mrt (fromInteger a):rest
            -- (T.Empty, T.Empty) -> Left $ "Invalid curly bracket body `" `T.append` inside `T.snoc` '`'
              -- case intMaybe front of
              --   Nothing -> Left $ "Invalid curly bracket body `" `T.append` inside `T.snoc` '`'
              --   Just a  -> return $ Mrt a:rest
            (T.Empty, back) -> do
              b <- intMaybe (T.drop 1 back)
              return $ Btw 0 (fromInteger b):rest
              -- case intMaybe back of
              --   Nothing -> Left $ "Invalid curly bracket body `" `T.append` inside `T.snoc` '`'
              --   Just b  -> return $ Btw 0 b:rest
            (front, back) -> do
              a <- intMaybe front
              b <- intMaybe (T.drop 1 back)
              return $ Btw (fromInteger a) (fromInteger b):rest
              -- case (intMaybe front, intMaybe back) of
              --   (Just a, Just b) -> return $ Btw a b:rest
              --   _ -> Left $ "Invalid curly bracket body `" `T.append` inside `T.snoc` '`'
        else do
          a <- intMaybe inside
          return $ Times (fromInteger a):rest
          -- case intMaybe inside of
          --   Nothing -> Left $ "Invalid curly bracket body `" `T.append` inside `T.snoc` '`'
            -- Just a -> return $ Times a:rest
  |
  x == '(' = do
    (inside, outside) <- splitInside 1 '(' txt
    rest <- tokenize outside
    body <- tokenize inside
    return $ Paren body:rest
    -- case splitInside 1 '(' txt of
    --   Nothing -> Left "Unclosed parentheses `"
      -- Just (inside, outside) -> do
        -- rest <- tokenize outside
        -- body <- tokenize inside
        -- return $ Paren body:rest
  |
  x == '[' = do
    -- Sketchy
    (inside, outside) <- splitInside 1 '[' txt
    let body = replaceNamedRanges inside
    rest <- tokenize outside
    if x == '^'
    then return $ Bracket True body:rest
    else return $ Bracket False (x T.:< body):rest
    -- case splitInside 1 '[' txt of
    --   Nothing -> Left $ "Unclosed bracket"
    --   Just (f, back) -> do
    --     let front = replaceNamedRanges f
    --     rest <- tokenize back
    --     if x == '^'
    --     then return $ Bracket True front:rest
    --     else return $ Bracket False (x T.:< front):rest
  |
  x == '\\' =
  -- Could be an anchor or a backref or an escape character
    case T.span (flip T.elem digit) txt of
      (T.Empty, _) -> do
        (c, back) <- T.uncons txt
        rest <- tokenize back
        return $
          (
          case c of
            'b' -> Anch EdgeWord
            'B' -> Anch NotEdgeWord
            '<' -> Anch BeginWord
            '>' -> Anch EndWord
            'w' -> Bracket True ('_' `T.cons` alnum)
            'W' -> Bracket False ('_' `T.cons` alnum)
            's' -> Bracket True space
            'S' -> Bracket False space
            _   -> Lit (toEsc c)
          ):rest
        -- case T.uncons txt of
          -- Nothing -> Left "Unescaped backslash at the end of the text."
          -- Just (c, back) -> do
            -- rest <- tokenize back
            -- return $
            --   (
            --     case c of
            --       'b' -> Anch EdgeWord
            --       'B' -> Anch NotEdgeWord
            --       '<' -> Anch BeginWord
            --       '>' -> Anch EndWord
            --       'w' -> Bracket True ('_' `T.cons` alnum)
            --       'W' -> Bracket False ('_' `T.cons` alnum)
            --       's' -> Bracket True space
            --       'S' -> Bracket False space
            --       _   -> Lit (toEsc c)
            --   ):rest
      (num, back) -> do
        br <- intMaybe num
        rest <- tokenize back
        return $ Back (fromInteger br):rest
        -- case intMaybe num of
        --   Nothing -> Left $ "Invalid number in backref `" `T.append` num `T.snoc` '`'
        --   Just br -> do
        --     rest <- tokenize back
        --     return $ Back br:rest
  |
  otherwise = do
    rest <- tokenize txt
    return $ Lit x:rest
tokenize T.Empty = return []
