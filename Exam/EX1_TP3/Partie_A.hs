module Partie_A where

import TP2

--cumule
cumule :: (a -> a -> a) -> a -> [a] -> a
cumule f n [] = n
cumule f n (x:xs) = (cumule f (f x n) xs)

--somme2
-- c'est la même fonction que dans le TP2
somme2 = somme2_1


--somme_num
somme_num :: Num p => [p] -> p
somme_num [] = 0
somme_num (x:xs) = cumule (+) x xs
