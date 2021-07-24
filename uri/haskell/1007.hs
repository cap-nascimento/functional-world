import Text.Printf

main = do
    a <- getLine
    b <- getLine
    c <- getLine
    d <- getLine
    let ans = (( (read a :: Integer) * (read b :: Integer) )-( (read c :: Integer) * (read d :: Integer) ))
    putStrLn ("DIFERENCA = " ++  (show ans))
