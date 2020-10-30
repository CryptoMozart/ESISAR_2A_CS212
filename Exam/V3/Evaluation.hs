module Evaluation where

import Types

eval :: Exp -> Float
eval (Val i) = i
eval (Op Plus exp1 exp2) = (eval exp1) + (eval exp2)
eval (Op Moins exp1 exp2) = (eval exp1) - (eval exp2)
eval (Op Fois exp1 exp2) = (eval exp1) * (eval exp2)
eval (Op Div exp1 exp2) = (eval exp1) / (eval exp2)
