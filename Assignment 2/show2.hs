import Data.Char
import Data.List

type Name = String
type Domain = [Integer]
type Valuation = [(Name,Integer)]

data Expr = Val Integer
	| Var Name
	| Expr :+: Expr
	| Expr :-: Expr
	| Expr :*: Expr
	| Expr :/: Expr
	| Expr :%: Expr

------- Exercise 1:	-----------------------------

par :: String -> String
par s = "(" ++ s ++ ")"

instance Show Expr where
	show (Val i) = show i
	show (Var x) = x
	show (p :+: q) = par(show p ++ "+" ++ show q)
	show (p :-: q) = par(show p ++ "-" ++ show q)
	show (p :*: q) = par(show p ++ "*" ++ show q)
	show (p :/: q) = par(show p ++ "/" ++ show q)
	show (p :%: q) = par(show p ++ "%" ++ show q)

------- Exercise 2:	-----------------------------

vars :: Expr -> [String]
vars expr = sort (remove (variables expr))

variables :: Expr -> [String]
variables (Val i) = []
variables (Var x) = [x]
variables (p :+: q) = (variables p) ++ (variables q)
variables (p :-: q) = (variables p) ++ (variables q)
variables (p :*: q) = (variables p) ++ (variables q)
variables (p :/: q) = (variables p) ++ (variables q)
variables (p :%: q) = (variables p) ++ (variables q)

remove :: [String] -> [String]
remove [] = []
remove (x:xs)
	| elem x xs = remove xs
	| otherwise = x : remove xs

------- Exercise 3:	-----------------------------

evalExpr :: Expr -> Valuation -> Integer
evalExpr e val
	| verify (vars e) (map fst val) = calc e val
	| otherwise = error "not all variables have values assigned"

verify :: [String] -> [Name] -> Bool
verify [] _ = True
verify (x:xs) ys
	| elem x ys = verify xs ys
	| otherwise = False

calc :: Expr -> Valuation -> Integer
calc (Val i) _ = i
calc (Var x) v = getVal x v 
calc (p :+: q) v = (calc p v) + (calc q v)
calc (p :-: q) v = (calc p v) - (calc q v)
calc (p :*: q) v = (calc p v) * (calc q v)
calc (p :/: q) v = div (calc p v) (calc q v)
calc (p :%: q) v = mod (calc p v) (calc q v)

getVal :: String -> Valuation -> Integer
getVal _ [] = error "variable is not in valuation --> verify is not correct"
getVal s (x:xs)
	| s == fst x = snd x
	| otherwise = getVal s xs

----------	valuations	-------------------------

valuations :: [(Name,Domain)] -> [Valuation]
valuations ((n,d):xs)
	| d == [] = []
	| otherwise = combine [(n, head d)] xs ++ valuations ([(n, tail d)] ++ xs)

combine :: Valuation -> [(Name,Domain)] -> [Valuation]
combine v [] = [v]
combine v ((n,d):xs)
	| xs == [] && d == [] = []
	| otherwise = [v ++ x | x <- valuations ((n,d):xs)]

----------	pytriples	-------------------------

pytriples :: Integer -> [Valuation]
pytriples n = validateTriples (computeTriples n)
	where
		validateTriples :: [Valuation] -> [Valuation]
		validateTriples xs = [x | x <- xs, isCorrect x, isPytriple x]

computeTriples :: Integer -> [Valuation]
computeTriples n = valuations [("a",[1..n]), ("b",[1..n]), ("c",[1..n])]

isCorrect :: Valuation -> Bool
isCorrect val = snd (head val) <= snd (head (tail val))

isPytriple :: Valuation -> Bool
isPytriple val = evalExpr e1 val == evalExpr e2 val
	where { e1 = (Var "a" :*: Var "a") :+: (Var "b" :*: Var "b");
			e2 = (Var "c" :*: Var "c")}

------- Exercise 5:	-----------------------------
toExpr :: String -> Expr
toExpr s = fst (parser s)

parser :: String -> (Expr,[String])
parser str = parseE (tokenize str)

parseE:: [String] -> (Expr,[String])
parseE tokens = parseE' acc rest
	where (acc,rest) = parseT tokens

parseE' :: Expr -> [String] -> (Expr,[String])
parseE' accepted ("+":tokens) = parseE' (accepted :+: acc) rest
	where (acc,rest) = parseT tokens
parseE' accepted ("-":tokens) = parseE' (accepted :-: acc) rest
	where (acc,rest) = parseT tokens
parseE' accepted tokens = (accepted, tokens)
	
parseT:: [String] -> (Expr,[String])
parseT tokens = parseT' acc rest
	where (acc,rest) = parseF tokens

parseT' :: Expr -> [String] -> (Expr,[String])
parseT' accepted ("*":tokens) = parseT' (accepted :*: expr) leftover
	where (expr,leftover) = parseF tokens
parseT' accepted ("/":tokens) = parseT' (accepted :/: expr) leftover
	where (expr,leftover) = parseF tokens
parseT' accepted ("%":tokens) = parseT' (accepted :%: expr) leftover
	where (expr,leftover) = parseF tokens
parseT' accepted tokens = (accepted, tokens)

parseF :: [String] -> (Expr,[String])
parseF (tok:tokens)
	| areLetters tok = (Var tok, tokens)
	| areDigits tok = (Val (toNumber tok), tokens)
	| areBrackets tok = (fst(parseE(taking tokens)),(dropping tokens)) --an expression between brackets
	| otherwise = error "Parse error ... abort"
	
areLetters :: String -> Bool
areLetters "" = True
areLetters s
	| isAlpha(head s) = areLetters (tail s)
	| otherwise = False
	
areDigits :: String -> Bool
areDigits "" = True
areDigits s
	| isDigit(head s) = areDigits (tail s)
	| otherwise = False
	
areBrackets :: String -> Bool
areBrackets s = (s == "(") || (s == ")")

toNumber :: String -> Integer
toNumber (x:[]) = toInteger (digitToInt x)
toNumber (x:xs) = ((toInteger (digitToInt x))*10^(length xs)) + toNumber xs

--take all strings in list before element ")"
taking :: [String] -> [String]
taking (tok:tokens)
	| head tokens == ")" = [tok]
	| otherwise = [tok] ++ taking tokens

--take all strings in list after element ")"
dropping:: [String] -> [String]
dropping (tok:tokens)
	| tok == ")" = tokens
	| otherwise = dropping tokens

--transform a string expression into a list of tokens	
tokenize :: String -> [String]
tokenize "" = []
tokenize s
	| head s == ' ' = tokenize (tail s)
	| isNotOperator (head s) = [takeWhile isNotOperator s] ++ tokenize (dropWhile isNotOperator s)
	| otherwise = [[head s]] ++ tokenize (tail s)
	
isNotOperator :: Char -> Bool
isNotOperator c = not((c == '(') || (c == ')') || (c == '+') || (c == '-') || (c == '*') || (c == '/') || (c == '%') )
