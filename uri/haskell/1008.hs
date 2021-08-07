import Text.Printf

main = do
    a <- getLine
    b <- getLine
    c <- getLine
    let ans = (read b :: Float) * (read c :: Float)
    --putStrLn "NUMBER = " ++ a ++ "\nSALARY = U$ " ++ (show ans)
    printf "NUMBER = %s\nSALARY = U$ %.2f\n" a ans
