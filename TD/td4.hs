--Exercice 1
data Complexe = Const Float Float  deriving Show

partie_reel :: Complexe -> Float
partie_reel (Const r i) = r

partie_imaginaire :: Complexe -> Float
partie_imaginaire (Const r i) = i

somme_complexe :: Complexe -> Complexe -> Complexe
somme_complexe (Const r i) (Const r1 i1) =(Const (r+r1) (i+i1) )

produit_complexe :: Complexe -> Complexe -> Complexe
produit_complexe (Const r1 i1) (Const r2 i2) = (Const (r1*r2-i1*i2) (r1*i2+r2*i1))

--Exercice 2
data Liste = Vide | Cell Int Liste

vide :: Liste -> Bool
vide Vide  = True
vide (Cell _ liste) = False

premier :: Liste -> Int
premier Vide = 0
premire (Cell n l) = n

reste :: Liste -> Liste
reste Vide = Vide
reste (Cell _ l) = l

longueur :: Liste -> Int 
longueur Vide = 0
longueur (Cell n l) = 1 + (longueur l)

dernier :: Liste -> Int
dernier Vide = 0
dernier (Cell n l) = dernier l

--Exercice 3
data ChaineBits = Fin | Zero ChaineBits | Un ChaineBits

complement_1 :: ChaineBits -> ChaineBits
complement_1 Fin = Fin
complement_1 (Zero chaine) = (Un (complement_1 chaine)
complement_1 (Un chaine) = (Zero (complement_1 chaine)
