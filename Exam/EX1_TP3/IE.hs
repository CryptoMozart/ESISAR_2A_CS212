module IE where

import Agreable
import TP2
import Partie_A


data Int_ex = Moins_inf | I Int deriving Show

max_ie :: Int_ex -> Int_ex -> Int_ex
max_ie Moins_inf (I a) = (I a)
max_ie (I a) Moins_inf = (I a) 
max_ie Moins_inf Moins_inf = Moins_inf
max_ie (I a) (I b) = if (a>b) then (I a) else (I b)


instance Agreable Int_ex where
	neutre = Moins_inf --car max_ie Moins_inf (Ent x) = x
	operation a b = max_ie a b

--listes de test:
liste :: [Int_ex]
liste = [Moins_inf,Moins_inf,Moins_inf]
liste2 :: [Int_ex]
liste2 = [(I (-2)), Moins_inf, (I (-5))]

max_liste :: [Int] -> Int_ex
max_liste [] = I 0
max_liste l = cumule_agreable (map I l) 
