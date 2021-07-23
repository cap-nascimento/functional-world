main = do 
    a <- getLine
    b <- getLine
    let ans = (read a :: Integer) + (read b :: Integer)
    putStrLn ("X = " ++ (show ans))
