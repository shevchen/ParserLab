module Processor where

import Grammar
import Tree

availableTerms :: NonTerm -> Expr -> [Term]
availableTerms from to = let fst = first to in
  if Epsilon `elem` fst
    then follow from ++ filter (/= Epsilon) fst
    else fst

processRule :: [Term] -> [Expr] -> ([Child], [Term])
processRule left []                  = ([], left)
processRule [] _                     = error("Should never happen")
processRule left ((Left Epsilon):xs) = processRule left xs
processRule (t:ts) ((Left x):xs)     = if x == t
  then let (children, next) = processRule ts xs in
    ((Left x):children, next)
  else error("Wrong token: expected " ++ show x ++ ", found " ++ show t)
processRule ts ((Right x):xs) = let (tree, next) = process x ts in
  let (children, next2) = processRule next xs in
    ((Right (x, tree)):children, next2)

searchForRule :: NonTerm -> [Term] -> [[Expr]] -> ([Child], [Term])
searchForRule _ [] _             = error("Should never happen")
searchForRule _ (e:_) []         = error("Unexpected token: " ++ show e)
searchForRule from left ([]:xs)  = searchForRule from left xs
searchForRule from (t:ts) (x:xs) = if t `elem` (availableTerms from $ head x)
  then case processRule (t:ts) x of
    ([], left) -> ([Left Epsilon], left)
    other      -> other
  else searchForRule from (t:ts) xs 

process :: NonTerm -> [Term] -> (ParseTree, [Term])
process nonTerm left = let (children, next) = searchForRule nonTerm left (rules nonTerm) in
  (Node children, next)

processAll :: [Term] -> ParseTree
processAll text = let (tree, left) = process S text in
  case left of
    [EndOfLine] -> Node [Right (S, tree)]
    _           -> error("Some symbols were not parsed at all.")
