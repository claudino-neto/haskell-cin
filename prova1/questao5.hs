--Letra A
data Pilha a = Pilha [a] deriving (Show)

--Letra B
push :: a -> Pilha a -> Pilha a
push x (Pilha xs) = Pilha (x:xs)

pop :: Pilha a -> Pilha a
pop (Pilha []) = error "Erro: pilha vazia!"
pop (Pilha (_:xs)) = Pilha xs

top :: Pilha a -> a
top (Pilha []) = error "Erro: pilha vazia!"
top (Pilha (x:_)) = x
