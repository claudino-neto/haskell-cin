--Questão 1
dobro :: Int -> Int
dobro x = 2 * x

--Questão 2
quadruplo ::  Int -> Int
quadruplo x = 2 * dobro x

--Questão 3
poli2 :: Double -> Double -> Double -> Double -> Double
poli2 a b c x = a * x^2 + b * x + c

--Questão 4
ehPar :: Int -> Bool
ehPar n = n `mod` 2 == 0

parImpar :: Int -> String
parImpar n
    | ehPar n   = "par"
    | otherwise = "impar"

--Questão 5
maxThree :: Integer -> Integer -> Integer -> Integer
maxThree a b c = max (max a b) c

maxFour :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour a b c d = max (maxThree a b c) d

maxFour2 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour2 a b c d = max (max (max a b) c ) d

--Questão 6
quantosIguais :: Integer -> Integer -> Integer -> Integer
quantosIguais a b c 
    | a == b && b == c  = 3
    | a == b || a == c || b == c    = 2
    | otherwise = 0

--Questão 7
ehZero :: Integer -> Bool
ehZero 0 = True
ehZero _ = False

ehZero2 :: Integer -> Bool
ehZero2 n
    | n == 0 = True
    | otherwise = False

--Questão 8
sumTo :: Integer -> Integer
sumTo 0 = 0
sumTo n = n + sumTo(n - 1)

--Questão 9
potencia :: Integer -> Integer -> Integer
potencia _ 0 = 1
potencia n k = n * potencia n (k - 1)

--Questão 10
