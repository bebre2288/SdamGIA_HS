solve :: [Int] -> (Int, Int)
solve dat = ( length arg, maximum (map (uncurry (+)) arg) )
    where arg = solveH dat


solveH :: [Int] -> [(Int, Int)]
solveH dat = [  (a,b) | (a,b) <- zip dat (tail dat),
                (a `mod` 3 == 0 || b `mod` 3 == 0) && (a<mdav || b<mdav)]
                where mdav = mediumAverage dat

mediumAverage :: [Int] -> Int
mediumAverage arr = sum ct `div` length ct
    where ct = filter even arr

unwrap :: String -> [Int]
unwrap input = map read $ lines input

main :: IO ()
main = do
    fileData <- readFile "17.txt"
    print $ show $ solve $ unwrap fileData
