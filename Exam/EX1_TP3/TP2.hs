module TP2 where

--Somme 
somme_1 [] = 0
somme_1 (x:xs) = x + somme_1 xs

--Produit
produit_1 [] = 1
produit_1 (x:xs) = x*(produit_1 xs)

--Cumule
cumule_1 :: (Int -> Int -> Int) -> Int -> [Int] -> Int
cumule_1 f n [] = n
cumule_1 f n (x:xs) = (cumule_1 f (f x n) xs)

--Somme2
somme2_1 :: [Int] -> Int
somme2_1 [] = 0
somme2_1 l = cumule_1 (+) 0 l

--Produit2
produit2_1 :: [Int] -> Int
produit2_1 [] = 1
produit2_1 l = cumule_1 (*) 1 l
