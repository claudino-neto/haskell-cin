--Letra A
f :: [Int] -> [Int]
f [] = []  -- Caso base: lista vazia retorna vazia
f [_] = [] -- Lista com um único elemento retorna vazia
f (x:y:xs)
  | x == y    = y : f (y:xs)  -- Se `x` e `y` são iguais, adiciona `y` e continua com `y:xs`
  | otherwise = f (y:xs)      -- Se `x` e `y` são diferentes, continua sem adicionar

--Letra B
f2 :: [Int] -> [Int]
f2 xs = [y | (x, y) <- zip xs (tail xs), x == y]

--Bônus letra B
f3 :: [Int] -> [Int]
f3 xs = [xs !! i | i <- [1..length xs - 1], xs !! i == xs !! (i - 1)]