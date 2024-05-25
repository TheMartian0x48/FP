f :: Int -> [Int] -> [Int]
f n arr = [ x | i <- [0..(length arr  - 1)] , let x = arr !! i, k <- [1..n]]


main :: IO ()
main = getContents >>=
       mapM_ print. (\(n:arr) -> f n arr). map read. words
