import Text.Printf

main = do
    a <- getLine
    b <- getLine
    let ans = (((read a :: Double) * 3.5) + ((read b :: Double) * 7.5))
    printf "MEDIA = %.5f\n" (ans / (3.5 + 7.5))
