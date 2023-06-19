solve :: [Int] -> (Int, Int)
solve dat = ( length arg, maximum arg )
    where arg = solveH dat

solveH :: [Int] -> [Int]
solveH dat = [  (a+b) `div` 2 | (a,i) <- zip filtered [0..],
                        (b,j) <- zip filtered [0..],
                        i<j && elem (div (a+b) 2) dat ]
                where filtered = filter odd dat

unwrap :: String -> [Int]
unwrap input = map read $ lines input

result :: String -> (Int, Int)
result input = solve $ unwrap input

main :: IO ()
main = do
    fileData <- readFile "data/26.txt"
    print $ result fileData
