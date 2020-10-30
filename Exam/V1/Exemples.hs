module Exemples where

import Types
import Evaluation

ex1 :: Exp
ex1 = Op Plus (Op Moins (Val 3) (Val 1)) (Val 2)

--ex2 :: Exp
--ex2 = Op Fois (Op Plus (Val 5) (Val 12)) (Op Moins (Val 15) (Val 8))
