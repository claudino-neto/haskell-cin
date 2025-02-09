--Letra A
poli :: Integer -> Integer -> Integer -> (Integer -> Integer)
poli a b c = \x -> a * x^2 + b * x + c

--Letra B
listaPoli :: [(Integer, Integer, Integer)] -> [Integer -> Integer]
listaPoli = map (\(a, b, c) -> poli a b c)

--Letra C
appListaPoli :: [Integer -> Integer] -> [Integer] -> [Integer]
appListaPoli fs xs = zipWith (\f x -> f x) fs xs