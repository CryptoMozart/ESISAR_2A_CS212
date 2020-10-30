module Main where
import Parseur
import Evaluation
main = do
	s <- getLine
	let e = entree_expression_entier s
	putStrLn "Résultat:"
	print (eval e)

