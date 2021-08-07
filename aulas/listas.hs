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

numBorrowed :: Database -> Person -> Int
numBorrowed db person = length 

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

-- Exercicio implementar soma de matrizes