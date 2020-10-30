--absolue :: Int -> Int
absolue x = if x >= 0 then x else (-x)

signe2  n  | n>0 = 1
	   | n<0 = (-1)
	   | n==0 = 0

absolue2 n | n>=0 = n
	   | n<0 = (-n)
 

--signe :: Int -> Char
signe x = if x>=0 then '+' else '-'

--fibo :: Int -> Int
fibo n = fibo_aux n 0 1
	where   fibo_aux 0 a b = b
		fibo_aux n a b = fibo_aux (n-1) b (a+b)

fibo n | n<1 = 0
       | n==1 = 1
       | otherwise = fibo (n-1) + fibo (n-2)

--EX2:
type Point = (Int,Int)
data Dep = Haut | Bas | Droite | Gauche

vers_droite :: Dep -> Point -> Point
vers_gauche :: Dep -> Point -> Point
vers_haut :: Dep -> Point -> Point
vers_bas :: Dep -> Point -> Point

vers_droite Droite (x,y) = (x+1,y)
vers_gauche Gauche (x,y) = (x-1,y)
vers_haut Haut (x,y) = (x,y+1)
vers_bas Bas (x,y) = (x,y-1)


--Ex3:
et :: [Bool] -> Bool
et [] = True
et (x:xs) = x && et xs

concatener :: [[a]] -> [a]
concatener [] = []
concatener (xs:xss) = xs ++ concatener xss

--type Chaine = [Char]
--concatener2 :: [Chaine] -> Chaine

--Ex4
type Pile = [Int]
is_empty :: Pile -> Bool
is_empty p | p==[] = True
           | otherwise = False

push :: (Pile,Int) -> Pile
push (p,n) = p ++ [n]

pop:: Pile -> Pile
pop [] = []
pop (x:xs) = if (xs == []) then (pop []) else (x:pop xs)

top :: Pile -> Int
top p = last p

top2 :: Pile -> Int
top2 [] = 0
top2 (x:xs) = if (xs==[]) then x else top2 xs


--EX5
parties :: [a] -> [[a]]
parties [] = [[]]
parties (x:xs) = (parties xs) ++ (map(x:)(parties xs))

ajoute :: a -> [a] -> [a]
ajoute x l = x:l 
	--équivalent ) [x]++l
-- (ajoute x) est de type [a] -> [a]

nb_parties :: [a] -> Int
nb_parties l = 2 ^ (length l)


