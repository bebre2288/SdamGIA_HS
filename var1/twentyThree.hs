-- add 1
-- add 2
-- mul by 2

f :: Int -> Int -> Int
f a b
    | a == b = 1
    | a > b  = 0
    | otherwise = (f (a+1) b) + (f (a+2) b) + (f (a*2) b)

result :: Int
result = f 3 10 * f 10 12
