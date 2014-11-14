import List

-- Ejercicio 1

f :: [a] -> [a]
f [] = []
f (x:xs) = x : f xs

g :: Eq a => [a] -> [a]
g xs
  | xs == [] = []
  | otherwise = head xs : g (tail xs)

h :: Eq a => [a] -> [a]
h xs = if xs==[] then [] else xs

i :: a -> a
i xs = xs


-- Ejercicio 2

testea :: Eq c => (a->b->c) -> (a->b->c) -> [(a,b)] -> Bool
testea f g xs = (map (uncurry f) xs) == (map (uncurry g) xs)

-- Ejercicio 3. Usa una función auxiliar donde los dos últimos parámetros
-- representan la longitud del segmento ordenado actual y la longitud del
-- segmento más largo encontrado hasta el momento

mayor :: Ord a => [a] -> Integer
mayor [] = 0
mayor (x:xs) = mayor' x xs 1 1

mayor' :: Ord a => a -> [a] -> Integer -> Integer -> Integer
mayor' x [] act tot = max act tot
mayor' x (y:ys) act tot
  | x <= y    = mayor' y ys (act+1) tot
  | otherwise = mayor' y ys 1 (max act tot)


-- Ejercicio 4

data Ord a => Arbol a = Vacio | Nodo a Int (Arbol a) (Arbol a)

arbolVacio :: Ord a => Arbol a
arbolVacio = Vacio

estaVacio :: Ord a => Arbol a -> Bool
estaVacio Vacio = True
estaVacio arbol = False

anadir :: Ord a => a -> Arbol a -> Arbol a
anadir x Vacio = Nodo x 1 Vacio Vacio
anadir x (Nodo y n iz dr)
  | x > y     = Nodo y n iz (anadir x dr)
  | x < y     = Nodo y n (anadir x iz) dr
  | otherwise = Nodo y (n+1) iz dr

eliminar :: Ord a => a -> Arbol a -> Arbol a
eliminar x Vacio = Vacio
eliminar x (Nodo y n iz dr)
  | x > y        = Nodo y n iz (eliminar x dr)
  | x < y        = Nodo y n (eliminar x iz) dr
  | n > 1        = Nodo y (n-1) iz dr
  | estaVacio dr = iz 
  | otherwise    = Nodo minDr minDrN iz (eliminarTotal minDr dr)
  where (minDr,minDrN) = minimo dr

minimo :: Ord a => Arbol a -> (a,Int)
minimo (Nodo x n iz dr)
  | estaVacio iz = (x,n)
  | otherwise    = minimo iz

eliminarTotal :: Ord a => a -> Arbol a -> Arbol a
eliminarTotal x Vacio = Vacio
eliminarTotal x (Nodo y n iz dr)
  | x > y        = Nodo y n iz (eliminarTotal x dr)
  | x < y        = Nodo y n (eliminarTotal x iz) dr
  | estaVacio dr = iz 
  | otherwise    = Nodo minDr minDrN iz (eliminarTotal minDr dr)
  where (minDr,minDrN) = minimo dr


inOrden :: Ord a => Bool -> Arbol a -> [a]
inOrden b Vacio = []
inOrden b (Nodo x n iz dr) = inOrden b iz ++ x' ++ inOrden b dr
  where x' = if b then replicate n x else [x]


ordenaSinRepeticiones :: Ord a => [a] -> [a]
ordenaSinRepeticiones xs = inOrden False (foldr anadir Vacio xs)


-- Ejercicio 5. El esquema básico es igual para los dos casos, sólo se 
-- diferencian en cómo convertir cada línea al tipo adecuado y volver a String.
-- 
-- Usamos la función sort definida por defecto. Si no quisiéramos usarla,
-- podríamos definirla a partir de lo hecho en el ejercicio anterior: 
-- sort xs = inOrden True (foldr anadir Vacio xs)

ordena :: IO()
ordena = do contenido <- readFile "patata.txt"
            let nuevo = procesa contenido
            writeFile "ordenados.txt" nuevo
procesa :: String -> String
procesa xs = unlines (map show (sort datos))
  where datos :: [Integer]
        datos = map read (lines xs)


ordena2 :: IO()
ordena2 = do contenido <- readFile "patata.txt"
             let nuevo = procesa2 contenido
             writeFile "ordenados.txt" nuevo
procesa2 :: String -> String
procesa2 xs = unlines (map aString (sort datos))
  where datos :: [(Integer,String)]
        datos = map lee (lines xs)

lee :: String -> (Integer, String)
lee xs = (read izqda,drcha)
  where (izqda,drcha) = span esDeNumero xs

aString :: (Integer,String) -> String
aString (x,y) = show x ++ y

esDeNumero :: Char -> Bool
esDeNumero c = elem c ('-':['0'..'9'])


-- Ejercicio 6

sufijos :: [a] -> [[a]]
sufijos [] = [[]]
sufijos (x:xs) = (x:xs) : sufijos xs

prefijos :: [a] -> [[a]]
prefijos [] = [[]]
prefijos (x:xs) = [] : map (x:) (prefijos xs)

segmentos :: [a] -> [[a]]
segmentos xs = concat (map sufijos (prefijos xs))


-- Ejercicio 7. Asumimos matrices no vacías. Podrian usarse las funciones
-- predefinidas minimum, maximum y and.

totalMayor :: Ord a => [[a]] -> [[a]] -> Bool
totalMayor xss yss = miMinimum (concat xss) >= miMaximum (concat yss)

continuaMayor :: Ord a => [[a]] -> [[a]] -> Bool
continuaMayor xss yss = miAnd (zipWith (>=) (concat xss) (concat yss))


miMinimum :: Ord a => [a] -> a
miMinimum xs = foldr1 min xs

miMaximum :: Ord a => [a] -> a
miMaximum xs = foldr1 max xs

miAnd :: [Bool] -> Bool
miAnd xs = foldr (&&) True xs 

-- Ejercicio 8

pots :: Num a => a -> [a]
pots x = scanl (*) 1 (repeat x)

factoriales :: [Integer]
factoriales = scanl (*) 1 [1..]

fvsp :: Integer -> Int
fvsp n = length (takeWhile not (zipWith (<) (pots n) factoriales))