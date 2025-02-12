import Data.List (group, sort)

--Questão 1
findDifference :: (Eq a, Show a) => [a] -> [a] -> Maybe String
findDifference xs ys
    | length xs /= length ys = Just (show (length xs) ++ " /= " ++ show (length ys))
    | otherwise = case [(i, x, y) | (i, (x, y)) <- zip [0..] (zip xs ys), x /= y] of
        (i, x, y):_ -> Just (show x ++ " /= " ++ show y)
        [] -> Nothing

--Questão 2
data Vetor = Vetor Integer Integer Integer deriving Show
instance Eq Vetor where
    (Vetor x1 y1 z1) == (Vetor x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2

--Questão 3
instance Num Vetor where
    (Vetor x1 y1 z1) + (Vetor x2 y2 z2) = Vetor (x1 + x2) (y1 + y2) (z1 + z2)
    (Vetor x1 y1 z1) * (Vetor x2 y2 z2) = Vetor (x1 * x2) (y1 * y2) (z1 * z2)
    abs (Vetor x y z) = Vetor (abs x) (abs y) (abs z)
    signum (Vetor x y z) = Vetor (signum x) (signum y) (signum z)
    fromInteger n = Vetor n n n  -- Permite criar um vetor com `fromInteger`
    negate (Vetor x y z) = Vetor (-x) (-y) (-z)

--Questão 4
freqs :: (Eq a, Ord a) => [a] -> [(Int, a)]
freqs xs = [(length g, head g) | g <- group (sort xs)]

--Questão 5
data ITree = ILeaf | INode Int ITree ITree deriving Show
instance Eq ITree where
    ILeaf == ILeaf = True
    (INode v1 l1 r1) == (INode v2 l2 r2) = v1 == v2 && l1 == l2 && r1 == r2
    _ == _ = False