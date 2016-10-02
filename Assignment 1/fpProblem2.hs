takuzuStrings :: Integer -> [String]
takuzuStrings n = checkAll (reverse (produceStrings n ((2^n)-1)))

produceStrings :: Integer -> Integer -> [String]
produceStrings n i --where n is the length of the string and i is the number the bitstring represents
	| i == 0 	= [strings n i]
	| otherwise = [strings n i] ++ produceStrings n (i-1)

--convert number i into a bitstring
strings :: Integer -> Integer -> String
strings n i 
	| n == 0 				= ""
	| i - (2^(n-1)) >= 0 	= "1" ++ strings (n-1) (i-2^(n-1)) 
	| otherwise 			= "0" ++ strings (n-1) i 

checkAll :: [String] -> [String]
checkAll [] = []
checkAll (x : xs)
	| checkString x ' ' 0 	= [x] ++ checkAll xs
	| otherwise 			= checkAll xs

--check if there are no more than 2 ones or zeros in a row
checkString :: String -> Char -> Integer -> Bool
checkString s c amount
	| (head s) == c && amount == 2 	= False
	| tail s == "" 					= True
	| head s == c 					= checkString (tail s) c (amount+1)
	| otherwise 					= checkString (tail s) (head s) 1 

wrapper :: [String] -> [String]
wrapper (a:_) = takuzuStrings (read a::Integer)

main =  print . wrapper . words =<< getLine
