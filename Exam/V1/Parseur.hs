-- parseur d'expressions arithmétiques postfixes (sans parenthèses)

module Parseur where

import Types
import Data.Char(isSpace, isNumber)
import Data.List(takeWhile)

data Lexeme = L_nb String | L_op Oper deriving (Show)

parseur :: String -> [Lexeme]

parseur [] = []
parseur (c:s) | isSpace c = parseur (mange_blancs s)
parseur (c:s) | isNumber c = (L_nb i):parseur r
                     where (i, r) = lis_entier (c:s)
parseur ('-':c:s) | isNumber c = (L_nb ('-':i)):parseur r
                     where (i, r) = lis_entier (c:s)
parseur (c:s) | c == '+' = (L_op Plus):parseur s
parseur (c:s) | c == '-' = (L_op Moins):parseur s
parseur (c:s) | c == '*' = (L_op Fois):parseur s
parseur (c:s) = parseur s
                           
mange_blancs :: String -> String
mange_blancs (' ':s) = mange_blancs s
mange_blancs str = str

lis_entier :: String -> (String, String)
lis_entier s = span isNumber s

traduction :: [Exp] -> Lexeme -> [Exp]
traduction p (L_nb s) = (Val (read s)):p
traduction (a:b:p) (L_op o) = (Op o b a):p

lexemes_vers_exp :: [Lexeme] -> Exp
lexemes_vers_exp l = head (foldl traduction [] l)

entree_expression_entier :: String -> Exp
entree_expression_entier = lexemes_vers_exp . parseur
