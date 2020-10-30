module Types where

data Oper = Plus | Moins | Fois

montre_oper :: Oper -> String
montre_oper Plus = "+"
montre_oper Moins = "-"
montre_oper Fois = "*"

instance Show Oper where
  show = montre_oper

data Exp = Val Int | Op Oper Exp Exp deriving (Show)

