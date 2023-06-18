import Data.List

f :: Int -> [Int] -> Bool
f x arr = (not inn || (x^2 <= 100)) && (not (x^2 <= 64) || inn)
    where ifx = [x]
          inn = [x] `isInfixOf` arr 

recfoo :: [(Int, Int)]
recfoo = [ (a,b) | a <- [-100..100], b <- [-100..100],
                    and [f x [a..b] | x <- [-100..100]]]

result :: Int
result = b-a
    where (a,b) = head $ reverse $ sortBy (\(a1,b1) (a2,b2) -> compare (b1-a1) (b2-a2)) recfoo

main :: IO ()
main = do putStrLn $ show result
