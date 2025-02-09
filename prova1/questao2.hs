--Letra A
testaElementosA :: (a -> Bool) -> [a] -> Bool
testaElementosA _ [] = True  -- Uma lista vazia sempre satisfaz qualquer condição
testaElementosA f (x:xs)
  | f x       = testaElementosA f xs  -- Se `f x` for True, continua verificando
  | otherwise = False  -- Se encontrar um elemento que não satisfaz `f`, retorna False

--Letra B
testaElementosB :: (a -> Bool) -> [a] -> Bool
testaElementosB f xs = and (map f xs)

--Letra C
testaElementosC :: (a -> Bool) -> [a] -> Bool
testaElementosC f = foldr (\x acc -> f x && acc) True
