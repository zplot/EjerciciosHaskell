------------------------------------------
-- Ejercicio 1

ej1a n = reverse (ej1a' n)

ej1a' n
  | n < 10    = [n]
  | otherwise = n`mod`10 : ej1a' (n`div`10)

ej1b n = foldl f 0 (ej1a' n)
  where f x y = 10*x+y

-------------------------------------------

-- Ejercicio 2. Observación: Si quisiéramos
-- usar los símbolos +,-,* tendríamos que
-- incluir el tipo Natural en la clase Num

data Natural = Cero | SUC Natural
  deriving (Eq,Ord,Show,Read)

suma Cero y = y
suma (SUC x) y = SUC (suma x y)

resta Cero y = Cero
resta x Cero = x
resta (SUC x) (SUC y) = resta x y

prod Cero y = Cero
prod (SUC x) y = suma y (prod x y)

fact Cero = SUC Cero
fact (SUC x) = prod (SUC x) (fact x)

eleva x Cero = SUC Cero
eleva x (SUC y) = prod x (eleva x y)

-------------------------------------------
-- Ejercicio 3

data Temp = Kelvin Float | Celsius Float | Fahrenheit Float
  deriving (Show,Read)

ceroAbs :: Float
ceroAbs = -273.15

toKelvin :: Temp -> Temp
toKelvin (Celsius x) = Kelvin (x-ceroAbs)
toKelvin (Fahrenheit x) = Kelvin (((x-32)/1.8)-ceroAbs)
toKelvin t = t

toFahrenheit :: Temp -> Temp
toFahrenheit (Celsius x) = Fahrenheit (32+ x*1.8)
toFahrenheit (Kelvin x) = Fahrenheit (32+ (x+ceroAbs)*1.8)
toFahrenteit t = t

toCelsius :: Temp -> Temp
toCelsius (Kelvin x) = Celsius (x+ceroAbs)
toCelsius (Fahrenheit x) = Celsius ((x-32)/1.8)
toCelsius t = t

escala :: Temp -> String
escala (Kelvin x)     = "Kelvin"
escala (Celsius x)    = "Celsius"
escala (Fahrenheit x) = "Fahrenheit"

instance Eq Temp where
  x == y = x'==y'
    where Kelvin x' = toKelvin x
          Kelvin y' = toKelvin y

instance Ord Temp where
  compare x y = compare x' y'
    where Kelvin x' = toKelvin x
          Kelvin y' = toKelvin y

-------------------------------------------
-- Ejercicio 4

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

-- Función auxiliar que devuelve el menor elemento de
-- un árbol binario de búsqueda no vacío
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


-------------------------------------------
-- Ejercicio 5

treeSort :: Ord a => [a] -> [a]
treeSort xs = inOrden (foldr anadir creaVacio xs)

-------------------------------------------
-- Ejercicio 6

data Pila a = P [a]

creaPila :: Pila a
creaPila = P []

esPilaVacia :: Pila a -> Bool
esPilaVacia (P []) = True
esPilaVacia pila   = False

apilar :: a -> Pila a -> Pila a
apilar x (P xs) = P (x:xs)

cima :: Pila a -> a
cima (P (x:xs)) = x

desapilar :: Pila a -> Pila a
desapilar (P (x:xs)) = P xs

{-
invierte :: [a] -> [a]
invierte xs = ys
  where P ys = (foldl (flip apilar) creaPila xs)-}

-------------------------------------------
-- Ejercicio 7

data Eq a => Cjto a = Cj [a]

creaCjVacio :: Eq a => Cjto a
creaCjVacio = Cj []

anadeACjto :: Eq a => a -> Cjto a -> Cjto a
anadeACjto x (Cj xs)
  | elem x xs = Cj xs
  | otherwise = Cj (x:xs)

esCjVacio :: Eq a => Cjto a -> Bool
esCjVacio (Cj []) = True
esCjVacio cjto    = False

eliminaDeCjto :: Eq a => a -> Cjto a -> Cjto a
eliminaDeCjto x (Cj xs) = Cj (elim x xs)
  where elim x []     = []
        elim x (y:ys) 
          | x==y      = ys
          | otherwise = x:elim x ys 

estaEnCjto :: Eq a => a -> Cjto a -> Bool
estaEnCjto x (Cj xs) = elem x xs

elemsDeCjto :: Eq a => Cjto a -> [a]
elemsDeCjto (Cj xs) = xs

{-
eliminaRepeticiones :: Eq a => [a] -> [a]
eliminaRepeticiones xs = elemsDeCjto (foldr anadeACjto creaCjVacio xs) -}



-------------------------------------------
-- Ejercicio 8

data Ord a => Cjto' a = Cj' (Arbol a)

creaCjVacio' :: Ord a => Cjto' a
creaCjVacio' = Cj' creaVacio

anadeACjto' :: Ord a => a -> Cjto' a -> Cjto' a
anadeACjto' x (Cj' arb)
  | busca x arb = Cj' arb
  | otherwise = Cj' (anadir x arb)


esCjVacio' :: Ord a => Cjto' a -> Bool
esCjVacio' (Cj' arb) = estaVacio arb

eliminaDeCjto' :: Ord a => a -> Cjto' a -> Cjto' a
eliminaDeCjto' x (Cj' arb) = Cj' (eliminar x arb)

estaEnCjto' :: Ord a => a -> Cjto' a -> Bool
estaEnCjto' x (Cj' arb) = busca x arb

elemsDeCjto' :: Ord a => Cjto' a -> [a]
elemsDeCjto' (Cj' arb) = inOrden arb

{-
eliminaRepeticiones' :: Ord a => [a] -> [a]
eliminaRepeticiones' xs = elemsDeCjto' (foldr anadeACjto' creaCjVacio' xs) -}

-------------------------------------------
-- Ejercicio 9

primeroQueCumpla :: (a->Bool) -> [a] -> Maybe a
primeroQueCumpla p [] = Nothing
primeroQueCumpla p (x:xs)
  | p x       = Just x
  | otherwise = primeroQueCumpla p xs

-- Otra versión usando funciones de orden superior predefinidas:

primero :: (a->Bool) -> [a] -> Maybe a
primero p xs 
  | null cumplen = Nothing
  | otherwise    = Just (head cumplen)
  where cumplen = filter p xs


-------------------------------------------
-- Ejercicio 10

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

-- usando la familia fold:

inits' :: [a] -> [[a]]
inits' xs = foldr f [[]] xs
  where f x xss = []:map (x:) xss


-------------------------------------------
-- Ejercicio 11

quitaDups :: Eq a => [a] -> [a]
quitaDups [] = []
quitaDups [x] = [x]
quitaDups (x:y:zs)
  | x==y      = quitaDups (y:zs)
  | otherwise = x : quitaDups (y:zs)


-- usando la familia fold:
quita xs = foldr f [] xs 
 where f x [] = [x]
       f x (y:ys) 
         | x==y = (y:ys)
         | otherwise = (x:y:ys)


-------------------------------------------
-- Ejercicio 12

lineas :: String -> [String]
lineas [] = []
lineas xs
  | null dr   = [iz]
  | otherwise = iz : lineas (tail dr)
  where (iz,dr) = span (/='\n') xs


antiLineas :: [String] -> String
antiLineas xs = foldr f [] xs
  where f linea texto = linea++"\n"++texto

-------------------------------------------
-- Ejercicio 13

justifDr :: Int -> String -> String
justifDr n texto = unlines (map (justifDr1 n) (lines texto))

justifDr1 :: Int -> String -> String
justifDr1 n xs = blancos ++ xs
  where blancos = replicate (n-length xs) ' '

{-
procesa ancho = do texto <- readFile "patata.txt"
                   writeFile "patata2.txt" (justifDr ancho texto)-}


-------------------------------------------
-- Ejercicio 14

justifCen :: Int -> String -> String
justifCen n texto = unlines (map (justifCen1 n) (lines texto))

justifCen1 :: Int -> String -> String
justifCen1 n xs = blancos1 ++ xs ++ blancos2
  where nblancos  = n-length xs
        nblancosI = nblancos `div` 2
        blancos1  = replicate nblancosI ' '
        blancos2  = replicate (nblancos-nblancosI) ' '

{-
procesaC ancho = do texto <- readFile "patata.txt"
                    writeFile "patata2.txt" (justifCen ancho texto)-}



-------------------------------------------
-- Ejercicio 15


justifAmbos :: Int -> String -> String
justifAmbos n texto = unlines (map (justifAmbos1 n) (lines texto))

justifAmbos1 :: Int -> String -> String
justifAmbos1 n xs 
  | numHuecos < 1 = unwords pals
  | otherwise     = unwords (primera ++ pals1' ++ pals2')
  where pals           = words xs
        numHuecos      = length pals - 1
        nblancos       = n - (length (concat pals)) - numHuecos
        extraNormal    = nblancos `div` numHuecos
        numExtras      = nblancos `mod` numHuecos
        blancosNormal2 = replicate extraNormal ' '
        blancosNormal1 = ' ':blancosNormal2

        (primera,resto) = splitAt 1 pals
        (pals1,pals2)   = splitAt numExtras resto
        pals1'          = map (blancosNormal1++) pals1
        pals2'          = map (blancosNormal2++) pals2

{-
procesaA ancho = do texto <- readFile "patata.txt"
                    writeFile "patata2.txt" (justifAmbos ancho texto)-}


-------------------------------------------
-- Ejercicio 16

type Vector = [Float]

sumaVectores :: Vector -> Vector -> Vector
sumaVectores v1 v2 = zipWith (+) v1 v2

restaVectores :: Vector -> Vector -> Vector
restaVectores v1 v2 = zipWith (-) v1 v2

prodEscalar :: Vector -> Vector -> Float
prodEscalar v1 v2 = sum (zipWith (*) v1 v2)

-------------------------------------------
-- Ejercicio 17

type Matriz = [[Float]]

sumaMatrices :: Matriz -> Matriz -> Matriz
sumaMatrices m1 m2 = zipWith sumaVectores m1 m2

restaMatrices :: Matriz -> Matriz -> Matriz
restaMatrices m1 m2 = zipWith restaVectores m1 m2

-- La siguiente función asume que la matriz es rectangular
transpuesta :: Matriz -> Matriz
transpuesta [] = []
transpuesta [vector] = map (:[]) vector
transpuesta (v1:vs)  = zipWith (:) v1 (transpuesta vs)


-- prodVectMatriz asume que la matriz ya está transpuesta
prodVectMatriz :: Vector -> Matriz -> Vector
prodVectMatriz v m = zipWith prodEscalar (repeat v) m

productoMatrices :: Matriz -> Matriz -> Matriz
productoMatrices m1 m2 = zipWith prodVectMatriz m1 (repeat m2')
  where m2' = transpuesta m2

-------------------------------------------
-- Ejercicio 18

type Polinomio = [Float]

evalua :: Polinomio -> Float -> Float
evalua pol x = foldl (f x) 0 pol 
  where f x acum coef = acum*x + coef


-------------------------------------------
-- Ejercicio 20

sumatorio xs   = foldr (+) 0 xs
productorio xs = foldr (*) 1 xs
prodEsc xs ys  = sumatorio (zipWith (*) xs ys)
concatena xss  = foldr (++) [] xss
dejaPares xs   = filter even xs
dejaImpares xs = filter odd xs
maximo xs      = foldr1 max xs
minimo xs      = foldr1 min xs
mcdgen xs      = foldr1 gcd xs
mcmgen xs      = foldr1 mcm xs


mcm x y = (x * y) `div` (gcd x y)