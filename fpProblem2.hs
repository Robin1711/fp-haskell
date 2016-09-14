--takuzuStrings :: Int -> [String]
--takuzuStrings n xs
--	| for all i<-[0..n]
--		xs ++ produceStrings i


--produceStrings :: Int -> [String] -> [String]
--produceStrings n xs
--	| n == 0 = 
--	| for all i <- [0,1] 
--		produceStrings n-1 xs++[i]



check :: String -> Char -> Int -> Bool
check s c n
	| compare s "" = True
	| n == 3 = False
	| c == head s = check (tail s) c (n+1)
	| otherwise = check (tail s) c 0