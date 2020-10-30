module Types where
data Oper = Plus | Moins | Fois | Div
montre_oper :: Oper -> String
montre_oper Plus = "+"
montre_oper Moins = "-"
montre_oper Fois = "*"
montre_oper Div = "/"
data Exp = Val Float | Op Oper Exp Exp

