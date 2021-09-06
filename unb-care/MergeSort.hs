merge :: (Ord t) => [t] -> [t] -> (t -> t -> Bool) -> [t]
merge [] [] comparator = []
merge a  [] comparator = a
merge [] b  comparator = b
merge (a:as) (b:bs) comparator
    | comparator a b = a:merge as (b:bs) comparator
    | otherwise      = b:merge (a:as) bs comparator

mergesort :: (Ord t) => [t] -> (t -> t -> Bool) -> [t]
mergesort []     comparator = []
mergesort (a:[]) comparator = [a]
mergesort (a:as) comparator = 
    let middle = floor (fromIntegral (length (a:as))/2)
        firstHalf  = mergesort (take middle (a:as)) comparator
        secondHalf = mergesort (drop middle (a:as)) comparator
    in merge firstHalf secondHalf comparator