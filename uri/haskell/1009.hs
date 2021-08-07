import Text.Printf

main = do
    name <- getLine
    salary <- getLine
    sales <- getLine
    let ans = ((read sales :: Double) * 0.15) + (read salary :: Double)
    printf "TOTAL = R$ %.2f\n" ans
