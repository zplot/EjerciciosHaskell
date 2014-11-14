module Trees (
    Arbol (..),
    creaVacio, estaVacio

    ) where


{- 4. Define un tipo de datos adecuado para manejar árboles de búsqueda binarios
que no tengan elementos en las hojas pero sí en los nodos intermedios.
Define funciones adecuadas para crear un árbol vacío, para saber si un árbol
está vacío, para añadir un elemento, para eliminarlo y para hacer recorridos
en preOrden, inOrden y postOrden. -}

{-data Ord a => Tree a = Empty | Node a (Tree a) (Tree a)

createEmptyTree :: Tree a
createEmptyTree = Empty

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _ = False

addElement:: Ord a => Tree a -> a -> Tree a
addElement Empty x = Node x Empty Empty
addElement (Node x a b) y
    | x < y         = Node x a (addElement b y)
    | otherwise     = Node x (addElement a y) b-}


{-deleteElement:: Ord a => Tree a -> a -> Tree a
deleteElement Empty x = Empty
deleteElement (Node x a b) y
    | isElementinTheTree -}




data Ord a => Arbol a = AVacio | Nodo a (Arbol a) (Arbol a)

creaVacio :: Ord a => Arbol a
creaVacio = AVacio

estaVacio :: Ord a => Arbol a -> Bool
estaVacio AVacio = True
estaVacio arbol = False

busca :: Ord a => a -> Arbol a -> Bool
busca x AVacio = False
busca x (Nodo y iz dr)
  | x == y    = True
  | x > y     = busca x dr
  | otherwise = busca x iz

anadir :: Ord a => a -> Arbol a -> Arbol a
anadir x AVacio = Nodo x AVacio AVacio
anadir x (Nodo y iz dr)
  | x >= y    = Nodo y iz (anadir x dr)
  | otherwise = Nodo y (anadir x iz) dr

eliminar :: Ord a => a -> Arbol a -> Arbol a
eliminar x AVacio = AVacio
eliminar x (Nodo y iz dr)
  | x > y        = Nodo y iz (eliminar x dr)
  | x < y        = Nodo y (eliminar x iz) dr
  | estaVacio dr = iz
  | otherwise    = Nodo menorDr iz (eliminar menorDr dr)
  where menorDr = menor dr

-- Funci�n auxiliar que devuelve el menor elemento de
-- un �rbol binario de b�squeda no vac�o
menor :: Ord a => Arbol a -> a
menor (Nodo x iz dr)
  | estaVacio iz = x
  | otherwise    = menor iz


preOrden :: Ord a => Arbol a -> [a]
preOrden AVacio = []
preOrden (Nodo x iz dr) = x:preOrden iz ++ preOrden dr

inOrden :: Ord a => Arbol a -> [a]
inOrden AVacio = []
inOrden (Nodo x iz dr) = inOrden iz ++ [x] ++ inOrden dr


postOrden :: Ord a => Arbol a -> [a]
postOrden AVacio = []
postOrden (Nodo x iz dr) = postOrden iz ++ postOrden dr ++ [x]

