-- Aula 1 - Haskell Basico
allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n == m) && (m == p)

fatorialfat :: Int -> Int
fatorialfat n
    | (n == 0) || (n == 1) = 1
    | (n > 1) = n * (fatorialfat (n-1))

all4Equal :: Int -> Int -> Int -> Int -> Bool
-- Computações desnecessárias
-- all4Equal n m o p = allEqual n m o && allEqual m o p
all4Equal n m o p = allEqual n m o && (n == p)

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
    | allEqual a b c = 3
-- Comparações desnecessárias em (/=)
--    | ((a == b) && (b /= c)) || ((a /= b) && (a == c)) || ((a /= b) && (b == c)) = 2
    | ((a == b) || (a == c) || (b == c)) = 2
    | otherwise = 0

-- Aula 2 - Exercicios
sales :: Int -> Int
sales 0 = 1
sales 1 = 0
sales 2 = 10
sales 3 = 5
sales 4 = 10
sales 5 = 25
sales 6 = 12
sales 7 = 9

howManySales :: Int -> Int -> Int
howManySales s n
    | (n == 0) && (sales 0 == s) = 1
    | n == 0 = 0
    | n > 0 && (sales n == s) = 1 + (howManySales s (n-1))
    | n > 0 = howManySales s (n-1)

makeSpaces :: Int -> String
makeSpaces n
   | n == 0 = ""
   | n > 0 = (makeSpaces (n-1)) ++ " "

pushRightConcat :: Int -> String -> String
pushRightConcat n a = (makeSpaces (n)) ++ a

pushRightRecursive :: Int -> String -> String
pushRightRecursive 0 a = a
pushRightRecursive n a = (pushRightRecursive (n-1) a) ++ " "

pushRightConditional :: Int -> String -> String
pushRightConditional n a
    | n == 0 = a
    | n > 0 = " " ++ pushRightConditional (n-1) a

-- Aula 3 - Exercicios
salesFloat :: Int -> Float
salesFloat 0 = 15
salesFloat 1 = 5
salesFloat 2 = 13
salesFloat 3 = 98

sumSales :: Int -> Float
sumSales 0 = salesFloat 0
sumSales n = sumSales (n-1) + salesFloat n

averageSales :: Int -> Float
averageSales n = sumSales n / fromIntegral (n+1)

averageSalesPattern :: Int -> Float
averageSalesPattern 0 = salesFloat 0
averageSalesPattern n = ((averageSalesPattern (n-1) * fromIntegral n) + salesFloat n)

averageSalesGuard :: Int -> Float
averageSalesGuard n
    | n == 0 = salesFloat 0
    | n > 0 = ((averageSalesGuard (n-1) * fromIntegral n) + salesFloat n) / fromIntegral (n+1)

sumSquares x y = let sqX = x * x
                     sqY = y * y
                     in sqX + sqY

-- Extra
greatestCommonDivisor :: Int -> Int -> Int
greatestCommonDivisor a b =
    | b == 0 = a
    | otherwise = greatestCommonDivisor b (a%b)
