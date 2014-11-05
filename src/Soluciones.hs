module Soluciones (
    doble, factorial, fibonacci,
    fib, cuadPerf, numRaices,
    ej7, prob8, prob9, prob11, prob12,
    prob13a, prob13b, prob15, prob16, mcd, mcm, Racional(..), simrac, sumarac, mcd', Natural (..), suma,
    resta, producto, fact, elevado,

    ) where

doble :: Int -> Int
doble x = x + x

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)


fibonacci :: Integer -> Integer -> Integer -> Integer
fibonacci 0 x y = y
fibonacci n x y = fibonacci (n-1) (x + y) x

fib :: Integer -> Integer
fib n = fibonacci n 1 1

cuadPerf :: Integer -> Bool
cuadPerf x = raiz^2 == x
    where raiz = round (sqrt (fromIntegral x ))

numRaices :: Float -> Float -> Float -> Int
numRaices a b c
                 | discriminante > 0 = 2
                 | discriminante == 0 = 1
                 | discriminante < 0 = 0
            where discriminante = b * b - 4 * a * c

ej7 :: [a] -> [b] -> [c] -> [(a,b,c)]
ej7 (x:xs) (y:ys) (z:zs) = (x,y,z) : ej7 xs ys zs
ej7 _ _ _ = []

prob8 :: [Int] -> Int
prob8 [] = 0
prob8 [x] = x
prob8 (x:xs) = x * prob8 xs

prob9 :: Num a => [a] -> [a] -> a
prob9 [] ys = 0
prob9 xs [] = 0
prob9 (x:xs) (y:ys) = x*y + prob9 xs ys

prob11 :: [[a]] -> [a]
prob11 [[]] = []
prob11 [] = []
prob11 (x:xs) = x ++ prob11 xs



prob12 :: Ord a => [a] -> Bool
prob12 [] = True
prob12 [x] = True
prob12 (x1:x2:xs) = (x1 < x2) && prob12 (x2:xs)




prob13a :: [Int] -> [Int]
prob13a [] = []
prob13a (x:xs)
    | par x          = [x] ++ prob13a xs
    | otherwise      = prob13a xs
        where par x = (x `mod` 2) == 0


prob13b :: [Int] -> [Int]
prob13b [] = []
prob13b (x:xs)
    | impar x          = [x] ++ prob13b xs
    | otherwise      = prob13b xs
        where impar x = (x `mod` 2) == 1


prob15 :: Integer -> Integer
prob15 0 = 2
prob15 1 = 2
prob15 n = 1 + prob15 (n `div` 2)

prob16 :: Integer -> [Integer]
prob16 0 = [0]
prob16 1 = [1]
prob16 n = prob16 (n `div` 2) ++ [(n `mod` 2)]

-- Problema 21
mcd :: Integral n => n -> n -> n
mcd a 0 = a
mcd a b = mcd b (a `mod` b)

mcm :: Integral a => a -> a -> a
mcm a b = ( a * b ) `div`  ( mcd a b )



-- PROBLEMA 22

-- Definimos el tipo Rac para racionales
data Racional = Racional Integer Integer deriving (Show, Read, Eq)

-- Simplificamos racionales
simrac :: Racional -> Racional
simrac (Racional a b) = (Racional (a `div` (mcd a b)) (b `div` (mcd a b)))

-- Suma de racionales
sumarac :: Racional -> Racional -> Racional
sumarac (Racional a b) (Racional c d) = (Racional (a * d + b * c) (b * d))


-- PROBLEMA 25
mcd' :: Integral n => [n] -> n
mcd' [] = 11
mcd' [a] = a
mcd' [a,b] = mcd a b
mcd' (x:y:xs) = mcd' ((mcd x y):xs)

-- Hoja 2 PROBLEMA 2
-- Supón que definimos el tipo data Natural = Cero | SUC Natural. Dfene funciones para hacer la suma,
-- la resta propia, el producto, el factorial y para elevar un natural a otro.

data Natural = Cero | SUC Natural deriving (Show, Eq, Ord)

{-suma :: Natural -> Natural -> Natural
suma Cero Cero = Cero
suma a Cero = a
suma (SUC a) b = SUC (suma a b)
suma b a = suma a b-}

suma Cero y = y
suma (SUC x) y = SUC (suma x y)

resta :: Natural -> Natural -> Natural
resta Cero Cero = Cero
resta a Cero = a
resta Cero (SUC Cero) = Cero
resta (SUC a) (SUC Cero) = a
resta (SUC a) (SUC b) = resta a b

--producto :: Natural a => a -> a -> a
producto :: Natural -> Natural -> Natural
producto a Cero = Cero
producto (SUC a) b = suma (producto a b) b
producto a b = producto b a

fact :: Natural -> Natural
fact Cero = SUC Cero
fact (SUC Cero) = SUC Cero
fact (SUC a) = producto (fact a) (SUC a)

elevado :: Natural -> Natural -> Natural
elevado Cero a = Cero
elevado (SUC Cero) a = SUC Cero
elevado a (SUC Cero) = a
elevado a (SUC b) = producto a (elevado a b)

