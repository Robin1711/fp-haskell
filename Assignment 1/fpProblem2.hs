takuzuStrings :: Integer -> [String]
takuzuStrings n = checkAll (reverse (produceStrings n ((2^n)-1)))

produceStrings :: Integer -> Integer -> [String]
produceStrings n i
	| i == 0 = [strings i n]
	| otherwise = [strings i n] ++ produceStrings n (i-1)

strings :: Integer -> Integer -> String
strings i n
	| n==0 = ""
	| i - (2^(n-1)) >= 0 = "1" ++ strings (i-2^(n-1)) (n-1)
	| otherwise = "0" ++ strings i (n-1)

checkAll :: [String] -> [String]
checkAll [] = []
checkAll (x : xs)
	| checkString x ' ' 0 = [x] ++ checkAll xs
	| otherwise = checkAll xs

checkString :: String -> Char -> Integer -> Bool
checkString s l amount
	| (head s) == l && amount == 2 = False
	| tail s == "" = True
	| head s == l = checkString (tail s) l (amount+1)
	| otherwise = checkString (tail s) (head s) 1

wrapper :: [String] -> [String]
wrapper (a:_) = takuzuStrings (read a::Integer)

main =  print . wrapper . words =<< getLine
