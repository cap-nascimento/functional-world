import Text.Printf

powerFloat :: Double -> Int -> Double
powerFloat base exp
    | (exp == 0) = 1.0
    | otherwise = base * (powerFloat base (exp-1))

main = do
    r <- getLine
    let pi = 3.14159
    let ans = (4/3.0) * pi
    printf "VOLUME = %.3f\n" (ans * (powerFloat (read r :: Double) 3))
