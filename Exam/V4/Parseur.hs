module Parseur where
import Types
import Data.Char(isSpace, isNumber)
data Lexeme = L_nb String | L_op Oper
parseur :: String -> [Lexeme]
parseur [] = []
parseur (c:s) | isSpace c = parseur (mange_blancs s)
parseur (c:s) | isNumber c = (L_nb i):parseur r
                     where (i, r) = lis_flottant (c:s)
parseur ('-':c:s) | isNumber c = (L_nb ('-':i)):parseur r
                     where (i, r) = lis_flottant (c:s)
parseur (c:s) | c == '+' = (L_op Plus):parseur s
parseur (c:s) | c == '-' = (L_op Moins):parseur s
parseur (c:s) | c == '*' = (L_op Fois):parseur s
parseur (c:s) | c == '/' = (L_op Div):parseur s
parseur (c:s) = parseur s
mange_blancs :: String -> String
mange_blancs (' ':s) = mange_blancs s
mange_blancs str = str
traduction :: [Exp] -> Lexeme -> [Exp]
traduction p (L_nb s) = (Val (read s)):p
traduction (a:b:p) (L_op o) = (Op o b a):p
lexemes_vers_exp :: [Lexeme] -> Exp
lexemes_vers_exp l = head (foldl traduction [] l)
entree_expression_entier :: String -> Exp
entree_expression_entier = lexemes_vers_exp . parseur
liste = ['.',',','e','E','-']
filtre_flottant :: Char -> Bool
filtre_flottant c | ((isNumber c) == True) = True
		  | ((elem c liste) == True) = True
		  | otherwise = False
lis_flottant :: String -> (String, String)
lis_flottant s = span filtre_flottant s







