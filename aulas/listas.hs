-- Funcoes extra
isEven :: Int -> Bool
isEven n = (mod n 2) == 0

isDigit :: Char -> Bool
isDigit c = (c >= '0' && c <= '9')

-- Exercicios (funcoes sobre string - pag. 9)
double :: [Int] -> [Int]
double xs = [2*a|a <- xs]
--double [] = []
--double (a:as) = (2*a):double(as)

doubleIfEven :: [Int] -> [Int]
doubleIfEven xs = [2*a|a <- xs, isEven a]

member :: [Int] -> Int -> Bool
member [] _ = False
member (a:as) x = (x == a) || member as x

-- member list comprehension
-- memberComprehension :: [Int] -> Int -> Int
-- memberComprehension xs x = x == a | a <- xs

digits :: String -> String
digits st = [ch | ch <- st, isDigit ch]
--digits s
--    | s == "" = ""
--    | head s >= '0' && head s <= '9' = (head s):(digits (tail s))
--    | otherwise = digits (tail s)

sumPairs :: [(Int, Int)] -> [Int]
sumPairs lp = [a+b|(a,b) <- lp]
--sumPairs [] = []
--sumPairs (a:as) = (fst(a) + snd(a)):sumPairs(as)


-- Funcoes sobre listas
zip2 :: [t] -> [u] -> [(t, u)]
zip2 (a:as) (b:bs) = (a,b):zip2 as bs
zip2 (a:as) [] = []
zip2 [] (b:bs) = []
zip2 [] [] = []

firstDigit :: String -> Char
firstDigit st = case ( digits st ) of
                []     -> '\0'
                (a:as) -> a

-- Exercicios (mini banco de dados)

hasLoan :: Database -> Person -> Book -> Bool
hasLoan [] _ _ = False
hasLoan ((person, book):db) p b = (person == p && book == b) || hasLoan db p b

type Person = String
type Book = String
type Database = [(Person, Book)]

exampleBase = 
    [("Alice", "Postman Pat"),
     ("Anna", "All Alone"), 
     ("Alice", "Spot"), 
     ("Alice", "Spot"),
     ("Rory", "Postman Pat")]

-- retrieve books borrowed by a specific person
books :: Database -> Person -> [Book]
books db p
    | db == [] = []
    | fst (head db) == p = snd (head db) : books (tail db) p
    | otherwise = books (tail db) p

booksComprehension :: Database -> Person -> [Book]
booksComprehension db p = [book | (person, book) <- db, person == p]

booksPattern :: Database -> Person -> [Book]
booksPattern [] _ = []
booksPattern ((person, book):dbs) key
    | person == key = book:books dbs key
    | otherwise = books dbs key

-- retrieve persons
borrowers :: Database -> Book -> [Person]
borrowers db b
    | db == [] = []
    | snd (head db) == b = fst (head db) : borrowers (tail db) b
    | otherwise = borrowers (tail db) b

borrowersComprehension :: Database -> Book -> [Person]
borrowersComprehension db b = [person | (person, book) <- db, book == b]

-- check if a book is borrowed
borrowed :: Database -> Book -> Bool
borrowed [] _ = False
borrowed ((person, book):dbs) key = (book == key) || (borrowed dbs key)

-- return number of loans from a specific person
numBorrowed :: Database -> Person -> Int
numBorrowed db key
    | db == [] = 0
    | fst (head db) == key = 1 + numBorrowed (tail db) key
    | otherwise = numBorrowed (tail db) key

-- make loan
makeLoan :: Database -> Person -> Book -> Database
makeLoan db p b = (p, b):db

-- return loan
returnLoan :: Database -> Person -> Book -> Database
returnLoan db p b
    | db == [] = []
    | (head db) == (p, b) = returnLoan (tail db) p b
    | otherwise = (head db):returnLoan (tail db) p b

returnLoanPattern :: Database -> Person -> Book -> Database
returnLoanPattern [] _ _ = []
returnLoanPattern ((person, book):db) p b
    | (person, book) == (p, b) = (returnLoanPattern db p b)
    | otherwise = (person, book):returnLoanPattern db p b

returnLoanComprehension :: Database -> Person -> Book -> Database
returnLoanComprehension db p b = [(person, book) | (person, book) <- db, (person, book) /= (p, b)]

-- Exercicio maxFun
sales :: Int -> Int
sales n = n * 10

-- maxFun :: (Int -> Int) -> Int -> Int -> Int
-- maxFun f 0 = f 0
-- maxFun f n = maxi (maxFun f (n-1)) (f n)

-- maxSales :: Int -> Int
-- maxSales 0 = sales 0
-- maxSales n = maxi (maxSales (n-1)) sales n

-- Exercicio isCrescent

f :: Int -> Int
f x = (x*x)*(-1)

isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f 0 = True
isCrescent f x = (f x) >= (f (x-1)) && isCrescent f (x-1)

-- Exercicio implementar soma de matrizes

sumPair :: (Int, Int) -> Int
sumPair (x, y) = x + y

matrixSum :: [[Int]] -> [[Int]] -> [[Int]]
matrixSum [] [] = []
matrixSum (a:as) (b:bs) = map sumPair (zip a b):matrixSum as bs

--Multiplicacao de matrizes

transpose :: [[Int]] -> [[Int]]
transpose ([]:_) = []
transpose a = (map head a) : transpose (map tail a)

linearComb :: [Int] -> [[Int]] -> [Int]
linearComb a [] = []
linearComb a (b:bs) = foldr (+) 0 (zipWith (*) a b) : linearComb a bs

matrixMult :: [[Int]] -> [[Int]] -> [[Int]]
matrixMult [] b = []
matrixMult (a:as) b = (linearComb a (transpose b)) : matrixMult as b

-- Interseção
find :: Int -> [Int] -> Bool
find a [] = False
find a (b:bs) = (a == b) || find a bs

intersection :: [Int] -> [Int] -> [Int]
intersection a b = [x | x <- a, find x b]

-- Uniao com repeticao
union :: [Int] -> [Int] -> [Int]
union a b = a ++ b

-- Uniao sem repeticao
remove :: Int -> [Int] -> [Int]
remove n l = [e | e <- l, e /= n]

unique :: [Int] -> [Int] -> [Int]
unique [] [] = []
unique [] b = b
unique (a:as) [] = unique as (a:remove a as)
unique (a:as) b = unique as (a:remove a (union b as))

unionNoRep :: [Int] -> [Int] -> [Int]
unionNoRep a b = unique (union a b) []
