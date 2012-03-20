module Grammar where

data Term = VarWord | Variable
          | Semicolon | Colon | Comma
          | Epsilon | EndOfLine
  deriving (Show, Eq)

data NonTerm = S | E | F | A
  deriving Show

type Expr = Either Term NonTerm

rules :: NonTerm -> [[Expr]]
rules S = [[Left VarWord, Right F, Left Semicolon, Right E]]
rules E = [[Right F, Left Semicolon, Right E], [Left Epsilon]]
rules F = [[Left Variable, Right A, Left Colon, Left Variable]]
rules A = [[Left Comma, Left Variable, Right A], [Left Epsilon]]

first :: Expr -> [Term]
first (Right S) = [VarWord]
first (Right E) = [Variable, Epsilon]
first (Right F) = [Variable]
first (Right A) = [Comma, Epsilon]
first (Left x)  = [x]

follow :: NonTerm -> [Term]
follow S = [EndOfLine]
follow E = [EndOfLine]
follow F = [Semicolon]
follow A = [Colon]

resWords      = ["var"  , ","  , ":"  , ";"      , "n"     , ""     ]
resWordsTerms = [VarWord, Comma, Colon, Semicolon, Variable, Epsilon]

maybeChoose :: (a -> Bool) -> [a] -> [b] -> b -> b
maybeChoose _ [] _ def          = def
maybeChoose _ _ [] def          = def
maybeChoose p (x:xs) (y:ys) def = if p x
  then y
  else maybeChoose p xs ys def

pickTermWord :: Term -> String
pickTermWord term = maybeChoose (== term) resWordsTerms resWords $ error("Not a term")::String
