-- Solution by Anderson Vieira
-- Try by yourself

import Data.Char

slice :: Int -> Int -> [t] -> [t]
slice i j xs 
    | i > 0 = take (j-i) $ drop (i) xs
    | otherwise = take (j-i) xs

myLength :: [t] -> Int
myLength [] = 0
myLength (a:at) = 1 + myLength at

isCharacter :: Char -> Bool
isCharacter c
    | (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') = True
    | otherwise = False

myReverse :: String -> String -> String
myReverse "" x  = x
myReverse (a:as) x = myReverse as ([a] ++ x)

shiftCharacters :: [(Char, Bool)] -> Int -> Bool -> [(Char, Bool)]
shiftCharacters [] _ _ = []
shiftCharacters ((c, flag):as) n f
    | flag && not f = (chr (ord c + n), True):shiftCharacters as n f
    | flag && f = (chr (ord c - n), True):shiftCharacters as n f
    | not flag = (c, False):shiftCharacters as n f

classify :: String -> [(Char, Bool)]
classify "" = []
classify (a:as)
    | isCharacter a = (a, True):classify as
    | otherwise = (a, False):classify as

reset :: [(Char, Bool)] -> [(Char, Bool)]
reset [] = []
reset ((c, flag):as) = (c, True):reset as

printArr :: [(Char, Bool)] -> String
printArr [] = ""
printArr ((c, flag):as) = c:printArr as

solve :: IO()
solve = do
    s <- getLine
    let len = myLength s
        rev = myReverse s ""
        sft = shiftCharacters (classify rev) 3 False
        mid = div len 2

        p1 = (slice 0 mid sft)
        p2 = (shiftCharacters (slice mid len (reset sft)) 1 True)

    putStrLn (printArr p1 ++ printArr p2)
    

testLoop :: Int -> IO()
testLoop 0 = return ()
testLoop n = do
    solve
    testLoop (n-1)

main = do
    t <- getLine
    testLoop (read t :: Int)