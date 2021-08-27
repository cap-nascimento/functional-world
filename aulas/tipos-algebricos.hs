data Expr = Lit Int         |
            Add Expr Expr   |
            Sub Expr Expr

-- Exercicio: Defina as funcoes
showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add e1 e2) = "(" ++ (showExpr e1) ++ " + " ++ (showExpr e2) ++ ")"
showExpr (Sub e1 e2) = "(" ++ (showExpr e1) ++ " - " ++ (showExpr e2) ++ ")"

data List t = Nil | Cons t (List t) deriving (Ord, Eq, Show)

data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Ord, Eq, Show)

toList :: List t -> [t]
toList Nil = []
toList (Cons n list) = n:toList list

fromList :: [t] -> List t
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)
-- Cons 45 (Cons 12 (Cons 35 Nil))

depth :: Tree t -> Int
depth NilT = 0
depth (Node t left right) = max (1 + (depth left)) (1 + (depth right))

-- (Node 1 (Node 2 NilT NilT) (Node 3 (Node 4 NilT NilT) NilT))

collapseIn :: Tree t -> [t]
collapseIn NilT = []
collapseIn (Node n left right) = (collapseIn left) ++ [n] ++ (collapseIn right)

--collapsePre :: Tree t -> [t]
--collapsePost :: Tree t -> [t]

mapTree :: (t -> u) -> Tree t -> Tree u
mapTree f NilT = NilT
mapTree f (Node n left right) = (Node (f n) (mapTree f left) (mapTree f right))