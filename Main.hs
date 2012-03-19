import Data.Char (isAlphaNum, isSpace)
import System.Environment (getArgs)
import qualified Control.Exception as CE
import System.Directory (doesFileExist)

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

printTree :: ParseTree -> String
printTree (Node [])                = ""
printTree (Node ((Left term):xs))  = show term ++ " " ++ printTree (Node xs)
printTree (Node ((Right tree):xs)) = "( " ++ printTree tree ++ ") " ++ printTree (Node xs)

availableTerms :: NonTerm -> Expr -> [Term]
availableTerms from to = let fst = first to in
  if Epsilon `elem` fst
    then follow from ++ filter (/= Epsilon) fst
    else fst

processRule :: [Term] -> [Expr] -> ([Child], [Term])
processRule left []                  = ([], left)
processRule [] _                     = error("Token expected but not found")
processRule left ((Left Epsilon):xs) = processRule left xs
processRule (t:ts) ((Left x):xs)     = if x == t
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
  then case processRule (t:ts) x of
    ([], left) -> ([Left Epsilon], left)
    other      -> other
  else searchForRule from (t:ts) xs 

process :: NonTerm -> [Term] -> (ParseTree, [Term])
process nonTerm left = let (children, next) = searchForRule nonTerm left (rules nonTerm) in
  (Node children, next)

delphiTypes = ["byte", "shortint", "word", "smallint", "longword", "cardinal", "longint", "integer", "int64", "single", "currency", "double", "extended", "char", "widechar", "ansichar", "shortstring", "string", "ansistring", "widestring", "boolean"]

resWords      = ["var"  , ","  , ":"  , ";"      ]
resWordsTerms = [VarWord, Comma, Colon, Semicolon]

wordsToTerm :: [String] -> [Term]
wordsToTerm []     = [EndOfString]
wordsToTerm (x:xs) = (tryResAndTypes x resWords resWordsTerms):(wordsToTerm xs)

tryResAndTypes :: String -> [String] -> [Term] -> Term
tryResAndTypes x [] _          = if x `elem` delphiTypes
  then Type
  else Variable

tryResAndTypes x (y:ys) (z:zs) = if x == y
  then z
  else tryResAndTypes x ys zs

stringToWords :: String -> [String]
stringToWords [] = []
stringToWords (x:xs) = if isSpace x
  then stringToWords xs
  else case span (\ c -> isAlphaNum c || c == '_') (x:xs) of
    ([], y:ys) -> if y `elem` ",:;"
      then [y]:(stringToWords ys)
      else error("Unexpected symbol: " ++ [y])
    (s, other) -> s:(stringToWords other)

main :: IO ()
main = do
  args <- getArgs
  if length args < 2 then putStrLn "Please specify input and output files." else do
  let input = head args in do
  inExists <- doesFileExist input
  if not inExists then putStrLn "Input file does not exist." else do
  source <- readFile input
  CE.catch (writeFile (args !! 1) (printTree $ fst $ process S $ wordsToTerm $ stringToWords source))
    (\ e -> putStrLn $ show (e::CE.SomeException))
