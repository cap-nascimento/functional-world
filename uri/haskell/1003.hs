main = do
    a <- getLine
    b <- getLine
    let c = (read a :: Int) + (read b :: Int)
    putStrLn ("SOMA = " ++ (show c))

