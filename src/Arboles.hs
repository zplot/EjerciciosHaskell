module Arboles (
    Arbol (..),
    creaVacio,
    estaVacio

    ) where



    data Arbol = AVacio | Node Arbol Int Arbol deriving (Show, Eq)

    creaVacio :: Arbol
    creaVacio = AVacio

    estaVacio :: Arbol -> Bool
    estaVacio AVacio = True
    estaVacio arbol = False

