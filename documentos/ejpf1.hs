-- Soluciones a la hoja de ejercicios 1

ej1 :: Floating a => a -> a
ej1 r = pi*r^2

ej2 :: Integer -> Bool
ej2 n = fromInteger (floor x) == x
  where x = sqrt (fromInteger n)

ej3 :: Real a => a -> a -> a -> Int
ej3 a b c
  | d < 0     = 0
  | d == 0    = 1
  | otherwise = 2
  where d = b^2 - 4*a*c

ej7 :: [a] -> [b] -> [c] -> [(a,b,c)]
ej7 (x:xs) (y:ys) (z:zs) = (x,y,z) : ej7 xs ys zs
ej7 _ _ _ = []

ej8 :: Integral a => [a] -> a
ej8 [] = 1
ej8 (x:xs) = x * ej8 xs

ej9 :: Num a => [a] -> [a] -> a
ej9 [] ys = 0
ej9 xs [] = 0
ej9 (x:xs) (y:ys) = x*y + ej9 xs ys

ej10 :: [a] -> [a] -> [a]
ej10 [] ys = ys
ej10 (x:xs) ys = x : ej10 xs ys

ej11 :: [[a]] -> [a]
ej11 [] = []
ej11 (xs:xss) = xs ++ ej11 xss

ej12 :: Ord a => [a] -> Bool
ej12 [] = True
ej12 [x] = True
ej12 (x:y:zs) = (x < y) && ej12 (y:zs)

ej13a :: Integral a => [a] -> [a]
ej13a [] = []
ej13a (x:xs)
  | even x    = ej13a xs
  | otherwise = x : ej13a xs

ej13b :: Integral a => [a] -> [a]
ej13b [] = []
ej13b (x:xs)
  | even x    = x : ej13b xs
  | otherwise = ej13b xs

ej14 :: Integral a => [a] -> [a]
ej14 xs = alterna pares impares
  where pares   = ej13b xs
        impares = ej13a xs
  
-- Precondición: Las dos listas de entrada son de la misma longitud
alterna :: [a] -> [a] -> [a]
alterna [] [] = []
alterna (x:xs) (y:ys) = x : y : alterna xs ys

ej15 :: Integral a => a -> a
ej15 x = 1 + ej15' (abs x)
ej15' x
  | x < 2 = 1
  | otherwise = 1 + ej15' (x `div` 2)

ej16 :: Integral a => a -> [a] 
ej16 x = reverse (ej16' x)
ej16' x
  | x < 2     = [x]
  | otherwise = (x `mod` 2) : ej16' (x `div` 2)

ej17a :: Ord a => [a] -> [a]
ej17a [] = []
ej17a xs = primero : ej17a (quita primero xs)
  where primero = menor xs
        menor [x] = x
        menor (x:y:zs) = min x (menor (y:zs))
        
        quita x [] = []
        quita x (y:ys)
          | x == y = ys
          | otherwise = y : quita x ys 

ej17b :: Ord a => [a] -> [a]
ej17b [] = []
ej17b (x:xs) = ej17b menores ++ [x] ++ ej17b mayores  
  where (menores,mayores) = separa x xs
        separa x [] = ([],[])
        separa x (y:ys)
          | x < y     = (men,y:mas)
          | otherwise = (y:men,mas)
          where (men,mas) = separa x ys

ej18 :: Int -> [a] -> [a]
ej18 n xs = take n (reverse xs)

ej19 :: Int -> [a] -> [a]
ej19 n xs = drop n (reverse xs)

ej20 :: Int -> Int
ej20 n = (genFib 1 1) !! n

genFib :: Int -> Int -> [Int]
genFib m n = m : genFib n (m+n)

ej21a :: Integral a => a -> a -> a
ej21a n 0 = n
ej21a 0 m = m
ej21a n m 
  | n > m = ej21a m (n `mod` m)
  | otherwise = ej21a n (m `mod` n)
-- Otra definición algo mejor pero más larga de mcd sería:
mcd n m = mcd' (max n m) (min n m)
mcd' n 0 = n
mcd' n m = mcd' m (n `mod` m)


ej21b :: Integral a => a -> a -> a
ej21b n m = (n*m) `div` (ej21a n m)

ej22simp :: (Integer,Integer) -> (Integer,Integer)
ej22simp (a,b)
  | b < 0 = (-a', -b')
  | otherwise = (a',b')
  where a' = a `div` d
        b' = b `div` d
        d  = ej21a (abs a) (abs b)

sumaR :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
sumaR (a,b) (c,d) = ej22simp (a*d+b*c,b*d)

restaR :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
restaR (a,b) (c,d) = ej22simp (a*d-b*c,b*d)

prodR :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
prodR (a,b) (c,d) = ej22simp (a*c,b*d)

divR :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
divR (a,b) (c,d) = ej22simp (a*d,b*c)

ej23 :: (Integer,Integer) -> Integer -> (Integer,Integer)
ej23 (a,b) p
  | p < 0     = pot (b,a) (-p)
  | otherwise = pot (a,b) p

pot :: (Integer,Integer) -> Integer -> (Integer,Integer)
pot r 0 = (1,1)
pot r n = prodR r (pot r (n-1))

ej24a :: Ord a => [a] -> a
ej24a [x] = x
ej24a (x:y:zs) = max x (ej24a (y:zs))

ej24b :: Ord a => [a] -> a
ej24b [x] = x
ej24b (x:y:zs) = min x (ej24b (y:zs))

ej25a :: Integral a => [a] -> a
ej25a [x] = x
ej25a (x:y:zs) = ej21a x (ej25a (y:zs))
 
ej25b :: Integral a => [a] -> a
ej25b [x] = x
ej25b (x:y:zs) = ej21b x (ej25b (y:zs))
 