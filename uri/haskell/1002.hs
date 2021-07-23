import Text.Printf

main = do
    r <- getLine
    let pi = 3.14159
    let a = (read r :: Double) * (read r :: Double) * pi
    printf "A=%.4f\n" a

