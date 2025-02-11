import Data.Char (toUpper)
import Data.Char (isAlpha)

--Questão 1
length' :: [a] -> Integer
length' xs = sum (map (\_ -> 1) xs)

--Questão 2

--Letra A
uppers :: String -> String
uppers = map toUpper

--Letra B
doubles :: [Int] -> [Int]
doubles = map (*2)

--Letra C
centavosReais :: [Int] -> [Float]
centavosReais = map (\x -> fromIntegral x / 100.0)

--Questão 3

--Letra A
letras :: String -> String
letras = filter(isAlpha)

--Letra B
rmChar :: Char -> String -> String
rmChar c = filter (/= c)

--Letra C
acima :: Int -> [Int] -> [Int]
acima n = filter (> n)

--Letra D
desiguais :: [(Int, Int)] -> [(Int, Int)]
desiguais = filter(\(x,y) -> x /= y)

--Questão 4
--[ f x | x <− xs, p x] é equivalente a map f ( filter p xs)

--Letra A [toUpper c | c <− s, isAlpha c ]
exprA :: String -> String
exprA s = map toUpper (filter isAlpha s)

--Letra B [2 * x | x <- xs, x > 3]
exprB :: [Int] -> [Int]
exprB xs = map (*2) (filter (> 3) xs)

--Letra C [reverse s | s <- strs, even (length s)]
exprC :: [String] -> [String]
exprC strs = map reverse (filter (\s -> even (length s)) strs)

--Questão 5

--Letra A
productRec :: [Int] -> Int
productRec [] = 1  -- Caso base: lista vazia retorna 1 (neutro da multiplicação)
productRec (x:xs) = x * productRec xs  -- Multiplica `x` pelo produto do restante da lista

productFold :: [Int] -> Int
productFold = foldr (*) 1

--Letra B
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold = foldr (&&) True

--Letra C
concatRec :: [String] -> String
concatRec [] = ""
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [String] -> String
concatFold = foldr (++) ""