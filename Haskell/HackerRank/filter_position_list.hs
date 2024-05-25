g :: [Int] -> Bool -> [Int] -> [Int]
g [ ] _ acc = acc
g (x:xs) o acc
    | o = g xs False acc
    | otherwise = g xs True (x:acc)

f :: [Int] -> [Int]
f lst = reverse $ g lst True [ ]

main = do
	inputdata <- getContents
	mapM_ (putStrLn. show). f. map read. lines $ inputdata
