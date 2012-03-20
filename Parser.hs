module Parser where

import Grammar
import Data.Char (isAlphaNum, isSpace)

wordsToTerm :: [String] -> [Term]
wordsToTerm []     = [EndOfLine]
wordsToTerm (x:xs) = (maybeChoose (== x) (take 4 resWords) resWordsTerms Variable):(wordsToTerm xs)

stringToWords :: String -> [String]
stringToWords [] = []
stringToWords (x:xs) = if isSpace x
  then stringToWords xs
  else case span (\ c -> isAlphaNum c || c == '_') (x:xs) of
    ([], y:ys) -> if y `elem` ",:;"
      then [y]:(stringToWords ys)
      else error("Unexpected symbol: " ++ [y])
    (s, other) -> s:(stringToWords other)
