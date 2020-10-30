module Expression where

import Types

montre_exp :: Exp -> String
montre_exp (Val i) = show i
montre_exp (Op o e1 e2) = "(" ++ (montre_exp e1) ++ (show o) ++ (montre_exp e2) ++ ")"

montre_exp_arbre :: Exp -> String
montre_exp_arbre (Val i) = show i ++ "\n"
montre_exp_arbre (Op o e1 e2) = a ++ (borde_lignes "|" b) ++ (borde_une_ligne "|" c)
                           where
                              a = (show o) ++ "\n"
                              b = borde_une_ligne "--- "  (montre_exp_arbre e1 ++ "\n")
                              c = borde_une_ligne "--- "  (montre_exp_arbre e2)

-- Sépare une liste d'éléments terminés par le caractère fourni. 
-- Ce qui se trouve après la dernière apparition du caractère est perdu.
separe_gauche :: Char -> String -> [String]
separe_gauche c [] = []
separe_gauche c (x:xs) | (x == c)  = "\n":separe_gauche c xs
separe_gauche c (x:xs) | otherwise = ajoute_au_premier x (separe_gauche c xs)

ajoute_au_premier :: a -> [[a]] -> [[a]]
ajoute_au_premier x [] = []
ajoute_au_premier x (y:ys) = (x:y):ys

borde_lignes:: String -> String -> String
borde_lignes b s = concat $ map (b ++) (separe_gauche '\n' s)

borde_une_ligne :: String -> String -> String
borde_une_ligne b s = u ++ v 
                         where
                            u = b ++ (head t)
                            v = concat $ map (espaces ++) (tail t)
                            espaces = take (length b) (repeat ' ')
                            t = separe_gauche '\n' s

montre_exp_postfix:: Exp -> String 
montre_exp_postfix (Val i) = show i --Le cas où c'est une valeur Float--
montre_exp_postfix (Op o e1 e2) = montre_exp_postfix (e1) ++ " "  ++ montre_exp_postfix(e2) ++ " " ++ show(o)

montre_exp_prefix:: Exp -> String
montre_exp_prefix (Val i) = show i  
montre_exp_prefix (Op o e1 e2) =  "( " ++ "(" ++ show(o) ++ ")" ++ " " ++  montre_exp_prefix(e1) ++ " " ++ montre_exp(e2) ++ " )"
