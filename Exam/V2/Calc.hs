module Main where

import Parseur
import Expression
import Exemples
import Evaluation

--main = print ex1

main = do
	s <- getLine
	let e = entree_expression_entier s
	putStrLn "Résultat montre_exp_arbre:"
	putStrLn (montre_exp_arbre e)
	putStrLn "Résultat montre_ex:"
	putStrLn (montre_exp e)
	putStrLn "Résultat montre_exp_postfix:"
	putStrLn (montre_exp_postfix e) 
	putStrLn "Résultat montre_exp_prefix:"
	putStrLn (montre_exp_prefix e)
	putStrLn "Résultat eval:"
	print (eval e)

