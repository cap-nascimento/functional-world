import Text.Printf

main = do
    a <- getLine
    b <- getLine
    c <- getLine
    d <- getLine
    let ans = ((read a :: Double) * 2.0) + ((read b :: Double) * 3.0) + ((read c :: Double) * 5.0)
    printf "MEDIA = %.1f\n" (ans / 10.0)
