-- Aula 1 - Haskell Basico
allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n == m) && (m == p)

fatorialfat :: Int -> Int
fatorialfat n
    | (n == 0) || (n == 1) = 1
    | (n > 1) = n * (fatorialfat (n-1))

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal n m o p = allEqual n m o && allEqual m o p

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
    | allEqual a b c = 3
    | ((a == b) && (b /= c)) || ((a /= b) && (a == c)) || ((a /= b) && (b == c)) = 2
    | otherwise = 0

-- Aula 2 - Exercicios
makeSpaces :: Int -> String
makeSpaces n
   | n <= 0 = ""
   | otherwise = (makeSpaces (n-1)) ++ " "

pushRight :: Int -> String -> String
pushRight n a
   | n <= 0 = a
   | otherwise = a ++ (makeSpaces (n))

main = do
    let pi = read "3.14" :: Double
    let piInt = read pi :: Integer
    print piInt

