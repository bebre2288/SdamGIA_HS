import Data.List

-- P = [17, 40]  Q = [20, 57]

f :: Int -> [Int] -> Bool
f x arr = (ifx `isInfixOf` arr) || (not ((ifx `isInfixOf` [17..40]) && (ifx `isInfixOf` [20..57])) || (ifx `isInfixOf` arr))
    where ifx = [x]


recfoo :: (Int, Int) -> Int -> (Int, Int)
recfoo (a,b) mn
    | b-a > mn = (a,b)
    | all (==True) [f x [a..b] | x <- [-1000..1000]] = recfoo (a+1,b) (b-a)
    | otherwise = recfoo (a,b+1) mn


result :: Int
result = b-a-1
    where (a,b) = recfoo (0,0) 100000
