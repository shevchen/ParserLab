module Parser where

import Grammar
import Data.Char (isAlphaNum, isSpace)

wordsToTerm :: [(String, Int)] -> [(Term, Int)]
wordsToTerm []          = [(EndOfLine, -1)]
wordsToTerm ((s, n):xs) = (maybeChoose (== s) (take 4 resWords) resWordsTerms Variable, n):(wordsToTerm xs)

stringToWords :: String -> Int -> [(String, Int)]
stringToWords [] _       = []
stringToWords (x:xs) pos = if isSpace x
  then stringToWords xs $ pos + 1
  else case span (\ c -> isAlphaNum c || c == '_') (x:xs) of
    ([], y:ys) -> if y `elem` ",:;"
      then ([y], pos):(stringToWords ys $ pos + 1)
      else error("Unexpected symbol: " ++ [y] ++ " at char " ++ show pos)
    (s, other) -> (s, pos):(stringToWords other $ pos + length s)
