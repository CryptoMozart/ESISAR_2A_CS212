module Agreable where

import Partie_A
import TP2

class Agreable a where
	neutre :: a
	operation :: a -> a -> a

instance Agreable Int where
	neutre = 0
	operation a b = a + b

instance Agreable Bool where
	neutre = False -- neutre pour "ou" car X || False = X
	operation a b = a || b -- || operateur "ou" dans haskell

instance Agreable [a] where
	neutre = [] --élément neutre pour la concaténation car X ++ [] = []
	operation a b = a ++ b --concaténation de listes

cumule_agreable :: Agreable a => [a] -> a
cumule_agreable [] = neutre
cumule_agreable (x:xs) = operation x (cumule_agreable xs)

--création d'une liste de test pour cumule
l :: [Int]
l = [1,2]





