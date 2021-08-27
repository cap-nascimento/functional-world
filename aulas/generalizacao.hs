myconcat :: [[t]] -> [t]
myconcat xs = foldr (++) [] xs

-- Exercicios
-- funcoes sobre listas
-- retornar o quadrado dos elementos (map)
square :: [Int] -> [Int]
square l = map f l
           where f x = x*x

-- retornar a soma dos quadrados dos numeros (fold)
squareSum :: [Int] -> Int
squareSum l = foldr (+) 0 (square l)

-- manter lista dos maiores que zero (filter)
positives :: [Int] -> [Int]
positives l = filter isPositive l
            where isPositive x = x > 0

-- retornar o cubo dos elementos (map)

f2 :: Int -> Int
f2 x = x*x*x

cube :: [Int] -> [Int]
cube l = map f2 l

-- retornar o ultimo elemento de uma lista
lastElement :: [t] -> t
lastElement (a:as)
    | length as == 0 = a
    | otherwise = lastElement as

-- retornar o n-esimo da lista
nthElement :: Int -> [t] -> t
nthElement n l
    | n > 0 && n <= (length l) = l!!(n-1)

nthElementRecursive :: Int -> [t] -> t
nthElementRecursive 1 (a:as) = a
nthElementRecursive n (a:as) = nthElementRecursive (n-1) as
