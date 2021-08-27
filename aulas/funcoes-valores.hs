-- id :: t -> t
-- id x = x

-- Exemplos
twice :: (t -> t) -> (t -> t)
twice f = f . f

-- iter :: Int -> (t -> t) -> (t -> t)
-- iter 0 f = id
-- iter n f = f >.> iter (n-1) f

--addNum :: Int -> (Int -> Int)
-- addNum n = (\m -> n+m)

addNum n = (+n)

multiply :: Int -> Int -> Int
multiply a b = a*b

doubleList :: [Int] -> [Int]
doubleList = map (multiply 2)

-- fazer exercicio do slide 9
-- f :: t -> u -> v
-- f t = 

g u t = \x -> t (u x)
