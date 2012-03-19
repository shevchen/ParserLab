import Prelude

data Term = VarWord | Variable | Type
          | Semicolon | Colon | Period
          | Epsilon | EndOfString
  deriving Show

data NonTerm = S | E | F | A
  deriving Show

Expr = Either Term Nonterm

rules :: NonTerm -> [[Expr]]
rules S = [[Left VarWord, Right F, Left Semicolon, Right E]]
rules E = [[Right F, Left Semicolon, Right E], [Left Epsilon]]
rules F = [[Left Variable, Right A, Left Colon, Left Type]]
rules A = [[Left Period, Left Variable, Right A], [Left Epsilon]]

first :: Expr -> [Term]
first (Right S) = [VarWord]
first (Right E) = [Variable, Epsilon]
first (Right F) = [Variable]
first (Right A) = [Period, Epsilon]
first (Left x)  = [x]

follow :: NonTerm -> [Term]
follow S = [EndOfString]
follow E = [EndOfString]
follow F = [Semicolon]
follow A = [Colon]

Child = Either Term ParseTree

data ParseTree = Node [Child]
  deriving Show

availableTerms :: NonTerm -> Expr -> [Term]
availableTerms from to = let fst = first to in
  if Epsilon `elem` fst
    then follow from ++ filter (not Epsilon) fst
    else fst

processRule :: [Term] -> [Expr] -> ([Child], [Term])
processRule left [] = ([], left)
processRule [] _    = error("Not enough tokens")
processRule (t:ts) ((Left x):xs) = if x == t
  then let (children, next) = processRule ts xs in
    ((Left x):children, next)
  else error("Wrong token")
processRule ts ((Right x):xs) = let (tree, next) = process x ts in
  let (children, next2) = processRule next xs in
    (tree:children, next2)

searchForRule :: NonTerm -> [Term] -> [[Expr]] -> ([Child], [Term])
searchForRule _ [] _             = error("Should never happen")
searchForRule _ (e:_) []         = error("Unexpected token: " ++ e)
searchForRule from (t:ts) (x:xs) = if t `elem` (availableTerms from $ head x)
  then processRule (t:ts) x
  else searchForRule from (t:ts) xs 

process :: NonTerm -> [Term] -> (ParseTree, [Term])
process nonTerm left = let (children, next) = searchForRule nonTerm left (rules nonTerm) in
  (Node children, next)
