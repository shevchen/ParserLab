module Writer where

import Grammar
import Tree

printTree :: ParseTree -> String
printTree (Node [])                      = ""
printTree (Node ((Left term):xs))        = show term ++ " " ++ printTree (Node xs)
printTree (Node ((Right (nt, tree)):xs)) = "( " ++ (show nt) ++ ": " ++ printTree tree ++ ") " ++ printTree (Node xs)

printDot :: ParseTree -> String
printDot tree = "digraph parsed {\n" ++ fst (printDot' tree 0 Nothing) ++ "}"

printDotNode :: Expr -> Int -> Maybe Int -> String
printDotNode e n (Just p)   = "  v" ++ show p ++ " -> v" ++ show n ++ ";\n" ++ printDotNode e n Nothing
printDotNode (Left t) n _   = "  subgraph {\n    rank = max;\n" ++ "    v" ++ show n ++ " [label=\"" ++ pickTermWord t ++ "\" shape=box width=0.2];\n  };\n"
printDotNode (Right nt) n _ = "  v" ++ show n ++ " [label=\"" ++ show nt ++ "\"];\n"

printDot' :: ParseTree -> Int -> Maybe Int -> (String, Int)
printDot' (Node []) n _                      = ("", n)
printDot' (Node ((Left term):xs)) n p        = let (str, num) = printDot' (Node xs) (n + 1) p in
  ((printDotNode (Left term) n p) ++ str, num)
printDot' (Node ((Right (nt, tree)):xs)) n p = let (str, num) = printDot' tree (n + 1) (Just n) in
  let (str2, num2) = printDot' (Node xs) num p in
    ((printDotNode (Right nt) n p) ++ str ++ str2, num2)
