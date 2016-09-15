--takuzuStrings :: Int -> [String]
--takuzuStrings n xs
--	| for all i<-[0..n]
--		xs ++ produceStrings i


--produceStrings :: Int -> [String] -> [String]
--produceStrings n xs
--	| n == 0 = 
--	| for all i <- [0,1] 
--		produceStrings n-1 xs++[i]

produceStrings :: Int -> [String] -> [String]
produceStrings n xs
	| showIntAtBase 2 intToDigit n

check :: String -> Char -> Int -> Bool
check s c n
	| s == "" = True
	| n == 3 = False
	| c == head s = check (tail s) c (n+1)
	| otherwise = check (tail s) c 0
