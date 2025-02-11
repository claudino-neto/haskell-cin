import Data.List (filter)
--Questão 1
import Data.Char (toUpper, isAlpha)

paraMaiuscula :: String -> String
paraMaiuscula str = [toUpper c | c <- str]

paraMaiusculaFiltrado :: String -> String
paraMaiusculaFiltrado str = [toUpper c | c <- str, isAlpha c]

--Questão 2
divisores :: Integer -> [Integer]
divisores n
    | n <= 0 = []
    | otherwise = [x | x <- [1 .. n], n `mod` x == 0]

--Questão 3
menorLista :: [Int] -> Int
menorLista [] = error "Lista vazia!"
menorLista [x] = x
menorLista (x:xs) = min x (menorLista xs)

--Questão 4
measure :: [Int] -> Int
measure [] = -1
measure xs = length(xs)

--Questão 5
takeFinal :: Int -> [Int] -> [Int]
takeFinal n xs
    | n <= 0    = []  -- Se `n` for zero ou negativo, retorna lista vazia
    | n >= length xs = xs  -- Se `n` for maior que a lista, retorna a lista inteira
    | otherwise = drop (length xs - n) xs  -- Pega os últimos `n` elementos

--Questão 6
remove :: Integer -> [a] -> [a]
remove _ [] = [] -- Se a lista for vazia, retorna lista vazia
remove 0 (_:xs) = xs -- Se `n = 0`, remove o primeiro elemento
remove n (x:xs)
    | n < 0 = x:xs -- Se `n` for negativo, retorna a lista original
    | otherwise = x : remove (n - 1) xs -- Continua percorrendo até `n = 0`

--Questão 7
incrementaPrimeiro :: [Integer] -> Integer
incrementaPrimeiro [] = 0 -- se a lista estiver vazia
incrementaPrimeiro (x:_) = x + 1 

--Questão 8
incrementaDois :: [Integer] -> Integer
incrementaDois [] = 0
incrementaDois [x] = x
incrementaDois (x:y:_) = x + y

--Questão 9
produto :: [Integer] -> Integer
produto [] = 1
produto (x:xs) = x * produto xs

--Questão 10

-- Conta quantas vezes um número aparece na lista
count :: Integer -> [Integer] -> Int
count x xs = length (filter (== x) xs)

-- Filtra apenas os números que aparecem exatamente uma vez
unique :: [Integer] -> [Integer]
unique xs = [x | x <- xs, count x xs == 1]

--Questão 11
estaOrdenada :: [Integer] -> Bool
estaOrdenada [] = True          -- Lista vazia está ordenada
estaOrdenada [_] = True         -- Lista com um único elemento está ordenada
estaOrdenada (x:y:xs)
    | x <= y    = estaOrdenada (y:xs)  -- Continua verificando a sequência
    | otherwise = False                -- Se um elemento for maior que o próximo, retorna False