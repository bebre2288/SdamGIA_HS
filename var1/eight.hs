import Data.List

perms :: [String]
perms = permutations "MATVEY"

step1 :: [String]
step1 = filter (\x -> (head x /= 'Y')) perms

step2 :: [String] -> [String]
step2 words = [ word |
                        word <- words,
                        all (==True) [ ([l1,l2] /= "AE") | (l1, l2) <- zip word (tail word)] ]

result :: Int
result = length $ step2 step1

main :: IO ()
main = print result
