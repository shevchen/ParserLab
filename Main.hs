module Main where

import Grammar
import Writer
import Processor
import Parser
import qualified Control.Exception as CE
import Control.DeepSeq (deepseq)

main :: IO ()
main = do
  source <- getContents
  let output = printDot $ processAll $ wordsToTerm $ stringToWords source 0 in do
  CE.catch (output `deepseq` putStrLn output)
    (\ e -> putStrLn $ show (e::CE.SomeException))
