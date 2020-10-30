-- sommecarre :: [Int] -> Int
sommecarre a = sum ( map (^2) (filter even a))  

-- halve :: [a] -> ([a],[a])
halve a = splitAt  (div (length a) 2) a

-- somme :: Int -> Int
somme n = (div) ((*) n (n+1)) 2

-- fac :: Int -> Int
fac 0 = 1
fac n  = n * fac(n-1)

fibo 0 = 0
fibo 1 = 1
fibo n = fibo(n-1) + fibo(n-2)

{- fibonacci :: Int -> Int 
fibo2 ::  Int -> Int -> Int -> Int	
fibo2 n a b = if( n /= 0) then fibo2 n-1 b a+b else a+b 
fibonacci n =  fibo2 n 0 1 -}

rond f g x = f (g x)

