--Questão 1
--Letra A
type Projeto = (String, [String]) -- ou: data Projeto = Projeto String [String] deriving (Eq, Show)

--Letra B
criaProjeto :: Projeto -> [Projeto] -> [Projeto]
criaProjeto p@(id, _) projetos
    | any (\(pid, _) -> pid == id) projetos = projetos  -- Se já existir, mantém a lista
    | otherwise = p : projetos  -- Caso contrário, adiciona o novo projeto

--Letra C
equipeDoProjeto :: String -> [Projeto] -> Maybe [String]
equipeDoProjeto pid projetos = lookup pid projetos

--Letra D
naEquipe :: String -> String -> [Projeto] -> Bool
naEquipe pid pessoa projetos = case lookup pid projetos of
    Just equipe -> pessoa `elem` equipe
    Nothing -> False

--Letra E
acrescentarPessoa :: String -> String -> [Projeto] -> [Projeto]
acrescentarPessoa pid pessoa [] = []
acrescentarPessoa pid pessoa ((p, equipe):xs)
    | p == pid = (p, if pessoa `elem` equipe then equipe else pessoa : equipe) : xs
    | otherwise = (p, equipe) : acrescentarPessoa pid pessoa xs

--Questão 2
data Nat = Zero | Succ Nat deriving (Eq, Show)

--Letra A
int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat n = Succ (int2Nat (n - 1))

--Letra B
nat2Int :: Nat -> Int
nat2Int Zero = 0
nat2Int (Succ n) = 1 + nat2Int n

--Letra C
somNat :: Nat -> Nat -> Nat
somNat Zero n = n
somNat (Succ m) n = Succ (somNat m n)

--Letra D
somant :: Nat -> Nat -> Int
somant n1 n2 = nat2Int (somNat n1 n2)