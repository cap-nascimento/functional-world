import Text.Printf

splitString :: String -> [String]
splitString st = words st

main = do
    l <- getLine
    let p1 = (splitString l)
    l <- getLine
    let p2 = (splitString l)
    
    let p1v = (read (p1!!2) :: Float) * (read (p1!!1) :: Float)
    let p2v = (read (p2!!2) :: Float) * (read (p2!!1) :: Float)

    let ans = p1v + p2v

    printf "VALOR A PAGAR: R$ %.2f\n" ans

