module Tree where

import Grammar

type Child = Either Term (NonTerm, ParseTree)

data ParseTree = Node [Child]
  deriving Show
