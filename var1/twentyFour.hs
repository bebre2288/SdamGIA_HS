-- chairgamingnc: @makesuey watch the primeagen on yt, vid title json as code i tink

-- X Y Z
-- !not XZZY!
-- length
--
-- Input -> Count Lengths -> Print Max Lenght
import Data.List

isXZZY :: (Char, Char, Char, Char) -> Bool
isXZZY (a,b,c,d) = [a,b,c,d] == "XZZY"

subSts :: String -> [(Char, Char, Char, Char)]
subSts str = zipWith4 (,,,) str (tail str) (tail (tail str)) (tail( tail (tail str)))

result :: String -> Int
result str = answ (subSts str) 0

answ :: [(Char, Char, Char, Char)] -> Int -> Int
answ arr mx
    | not $ elem ('X','Z','Z','Y') arr = max (length arr + 3) (mx+2)
    | lnt > mx  = answ (drop lnt arr) lnt 
    | otherwise = answ (drop lnt arr) mx
        where lnt = length (takeWhile (not . isXZZY) arr ) + 1 


main :: IO ()
main = do
    input <- readFile "data/24.txt"
    print $ result input
