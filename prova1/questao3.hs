sublistas :: [a] -> [[a]]
sublistas [] = [[]]  -- Caso base: a única sublista de [] é []
sublistas (x:xs) = 
    let subs = sublistas xs  -- Gera todas as sublistas do restante da lista
    in subs ++ map (x:) subs  -- Adiciona `x` a cada sublista gerada
