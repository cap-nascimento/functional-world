comparatorInt :: Int -> Int -> Bool
comparatorInt a b = a <= b

quicksort :: [t] -> (t -> t -> Bool) -> [t]
quicksort [] comparator = []
quicksort (x:xs) comparator = [a | a <- xs, (comparator a x)] ++ [x] ++ [a | a <- xs, not (comparator a x)]

isSorted :: [t] -> (t -> t -> Bool) -> Bool
isSorted [] comparator         = True
isSorted (hd:[]) comparator    = True
isSorted (hd:nk:tl) comparator = (comparator hd nk) && isSorted (nk:tl) comparator

filterMatrix :: [[t]] -> (t -> Bool) -> [[t]]
filterMatrix [] comparator = []
filterMatrix (hd:tl) comparator = filter comparator hd : filterMatrix tl comparator

intersection :: (Eq t) => [t] -> [t] -> [t]
intersection [] []     = []
intersection [] b      = []
intersection (a:as) [] = []
intersection (a:as) b
    | a `elem` b = a:intersection as b
    | otherwise = intersection as b

removeDuplicates :: (Eq t) => [t] -> [t] -> [t]
removeDuplicates [] [] = []
removeDuplicates a [] = a
removeDuplicates a (b:bs)
    | not (b `elem` a) = removeDuplicates (b:a) bs
    | otherwise = removeDuplicates a bs

unionUnique :: (Ord t) => [t] -> [t] -> [t]
unionUnique a b = reverse (removeDuplicates [] (quicksort (a ++ b) (<)))

type Medicamento = String
type Quantidade = Int

data Cuidado = Comprar Medicamento Quantidade |
               Medicar Medicamento deriving Show


getCuidadoMedicamento :: Cuidado -> String
getCuidadoMedicamento (Comprar m _) = m
getCuidadoMedicamento (Medicar m)   = m

getCuidadoQuantidade :: Cuidado -> Int
getCuidadoQuantidade (Comprar _ q) = q

getCuidadoType :: Cuidado -> String
getCuidadoType (Comprar _ _) = "Comprar"
getCuidadoType (Medicar _)   = "Medicar"

getCuidado :: [Cuidado]
getCuidado = 
    [Comprar "Aspirina" 30, Medicar "Paracetamol", Medicar "Aspirina",
     Comprar "Diazepan" 12, Comprar "Cloroquina" 50,
     Medicar "Invermectina"]

getCuidadoMatrix :: [[Cuidado]]
getCuidadoMatrix =
    [[Comprar "Aspirina" 12, Comprar "Paracetamol" 32, Medicar "Aspirina"],
     [Medicar "Diazepan", Comprar "Cloroquina" 22, Comprar "Diazepan" 15],
     [Medicar "Invermectina", Medicar "Aspirina"]]

