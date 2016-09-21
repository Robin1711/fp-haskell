cipherEncode :: Int -> String -> String
cipherEncode r s
	| s == "" = s
	| otherwise = code r 1 s

cipherDecode :: Int -> String -> String
cipherDecode r s
	| s == "" = s
	| otherwise = code (-r) 1 s

code :: Int -> Int -> String -> String
code r n s
	| s == ""	= ""
	| (head s) == ' ' = " " ++ code r n (tail s)
	| otherwise = [rotateChar r n (head s)] ++ code r (n+1) (tail s)

rotateChar :: Int -> Int -> Char -> Char
rotateChar r n c
	| n == 0 = c
	| otherwise = rotateChar r (n-1) (rotate c r)

rotate :: Char -> Int -> Char
rotate c r
	| (num - r) > (fromEnum 'Z') = toEnum (num - r - 26)
	| (num - r) < (fromEnum 'A') = toEnum (num - r + 26)
	| otherwise = toEnum (num - r)
	where num = fromEnum c
	