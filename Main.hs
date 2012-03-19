data Term = VarWord | Variable | Type
          | Semicolon | Colon | Comma
          | Epsilon | EndOfString
  deriving (Show, Eq)

data NonTerm = S | E | F | A
  deriving Show

type Expr = Either Term NonTerm

rules :: NonTerm -> [[Expr]]
rules S = [[Left VarWord, Right F, Left Semicolon, Right E]]
rules E = [[Right F, Left Semicolon, Right E], [Left Epsilon]]
rules F = [[Left Variable, Right A, Left Colon, Left Type]]
rules A = [[Left Comma, Left Variable, Right A], [Left Epsilon]]

first :: Expr -> [Term]
first (Right S) = [VarWord]
first (Right E) = [Variable, Epsilon]
first (Right F) = [Variable]
first (Right A) = [Comma, Epsilon]
first (Left x)  = [x]

follow :: NonTerm -> [Term]
follow S = [EndOfString]
follow E = [EndOfString]
follow F = [Semicolon]
follow A = [Colon]

type Child = Either Term ParseTree

data ParseTree = Node [Child]
  deriving Show

availableTerms :: NonTerm -> Expr -> [Term]
availableTerms from to = let fst = first to in
  if Epsilon `elem` fst
    then follow from ++ filter (/= Epsilon) fst
    else fst

processRule :: [Term] -> [Expr] -> ([Child], [Term])
processRule left [] = ([], left)
processRule [] _    = error("Token expected but not found")
processRule (t:ts) ((Left x):xs) = if x == t
  then let (children, next) = processRule ts xs in
    ((Left x):children, next)
  else error("Wrong token: expected " ++ show x ++ ", found " ++ show t)
processRule ts ((Right x):xs) = let (tree, next) = process x ts in
  let (children, next2) = processRule next xs in
    ((Right tree):children, next2)

searchForRule :: NonTerm -> [Term] -> [[Expr]] -> ([Child], [Term])
searchForRule _ [] _             = error("Should never happen")
searchForRule _ (e:_) []         = error("Unexpected token: " ++ show e)
searchForRule from (t:ts) (x:xs) = if t `elem` (availableTerms from $ head x)
  then processRule (t:ts) x
  else searchForRule from (t:ts) xs 

process :: NonTerm -> [Term] -> (ParseTree, [Term])
process nonTerm left = let (children, next) = searchForRule nonTerm left (rules nonTerm) in
  (Node children, next)
