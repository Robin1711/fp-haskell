rpnEval:: String -> Int
rpnEval s = calc (snd (getNumber s 0)) (fst (getNumber s 0)) 0

calc :: String -> Int -> Int -> Int
calc s i j
	| (s == "")	= i
	| getCharacter s == -5 = calc (tail s) i j
	| getCharacter s < 0 = calc (tail s) (useOp i j (getCharacter s)) 0
	| getCharacter s >= 0 && getCharacter (tail s) >= 0 = calc (snd (getNumber s 0)) i (fst (getNumber s 0)) 
	| otherwise = calc (tail s) i (getCharacter s)

getNumber :: String -> Int -> (Int, String)
getNumber s i
	| not(s == "") && getCharacter s >= 0 = getNumber (tail s) (i*10 + getCharacter s)
	| otherwise 	= (i, s)

useOp :: Int -> Int -> Int -> Int
useOp i j o
	| o == -1 = i + j
	| o == -2 = i - j
	| o == -3 = i * j
	| o == -4 = div i j
	
getCharacter :: String -> Int
getCharacter s
	| charIsNum (head s) = charToNum (head s)
	| charIsOperator (head s) = charToOperator (head s)
	| otherwise = -5

charIsNum:: Char -> Bool
charIsNum ch
	|('0'<=ch)&&(ch<='9')	= True
	| otherwise				= False
	
charIsOperator:: Char -> Bool
charIsOperator ch
	|ch == '+'	= True
	|ch == '-'	= True
	|ch == '*'	= True
	|ch == '/'	= True
	| otherwise	= False

charToNum:: Char -> Int
charToNum ch = fromEnum ch - 48
	
charToOperator:: Char -> Int
charToOperator ch
	|ch == '+'	= -1
	|ch == '-'	= -2
	|ch == '*'	= -3
	|ch == '/'	= -4
