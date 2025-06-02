module Interpret.Parse where
-- module Interpret.Parse(parse) where
-- Only exposes parse

import Interpret.Data
import Misc
-- import Data.Map(Map)
-- import Data.Word
-- import Data.Either
-- import Data.Tree
-- Note that the tokenizer already grabs the operator mapping.

-- Should be the only exposed part of the module.
-- Transforms array of toks to ready-to-interpret OpTree (OpTree is from Data).
-- Somehow a tree is not getting made.
parse :: [Token] -> Expression
parse = (foldl' insert (Operand $ Tup [])) . bunch

-- Bunches things up to (Tuple) (Rank + Operator Map) (Tuple) (Rank + Operator Map) ...
bunch :: [Token] -> [Token]
bunch [] = []
-- Inserts Nones where there are two consecutive operators. Helps to make the tree not need to check.
bunch (op1@(Opr _ _):op2@(Opr _ _):xs) = op1:Tup []:(bunch $ op2:xs)
bunch (op@(Opr _ _):xs) = op:bunch xs
bunch xs = collapse front:bunch rest
  where
    (x, rest) = splitWith (\t -> case t of {Opr _ _ -> True; _ -> False}) xs
    front = Tup $ map (\t -> case t of {Tup ys -> collapse $ Tup (bunch ys); y -> y}) x

-- the input is always at the right, so if it's of equal rank,
-- it is done left to right. So equal would be equivalent to the right being of less rank.
-- This means we want to check for specifically if the left is of lower rank than the right.
-- If it is, then the right gets to dive in. If it isn't, then the right is put at the top.
insert :: Expression -> Token -> Expression
insert (Operand (Tup [])) x =
  case x of
    Opr r o -> Expression r o (Operand $ Tup []) (Operand $ Tup [])
    t       -> Operand t
insert expr@(Expression rank top left right) x =
  case x of
    Opr r o ->
      if rank < r
      then Expression rank top left $ insert right x
      else Expression r    o   expr   (Operand $ Tup [])
    _       ->
      Expression rank top left $ insert right x
insert left@(Operand opr) x =
  case x of
    Opr r o -> Expression r o left (Operand $ Tup [])
    Tup tup ->
      case opr of
        Tup t2 -> Operand $ Tup $ t2 ++ tup
        x'     -> Operand $ Tup $ x':tup
    _       ->
      case opr of
        Tup t2 -> Operand $ Tup $ t2 ++ [x]
        _      -> Operand $ Tup [opr, x]

-- The body of the tree function. Inserts children on the right,
-- may move parts of acc to be its left child based on rank.
treeify :: Expression -> [Token] -> Expression
treeify acc []     = acc
treeify acc (x:xs) = treeify (insert acc x) xs
