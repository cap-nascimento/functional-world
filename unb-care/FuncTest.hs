comparatorInt :: Int -> Int -> Bool
comparatorInt a b = a <= b

isSorted :: [t] -> (t -> t -> Bool) -> Bool
isSorted [] comparator         = True
isSorted (hd:[]) comparator    = True
isSorted (hd:nk:tl) comparator = (comparator hd nk) && isSorted (nk:tl) comparator