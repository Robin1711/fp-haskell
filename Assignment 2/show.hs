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

par :: String -> String
par s = "(" ++ s ++ ")"

instance Show Expr where
	show (Val i) = [intToDigit (fromInteger i)]
	show (Var x) = x
	show (p :+: q) = par(show p ++ "+" ++ show q)
	show (p :-: q) = par(show p ++ "-" ++ show q)
	show (p :*: q) = par(show p ++ "*" ++ show q)
	show (p :/: q) = par(show p ++ "/" ++ show q)
	show (p :%: q) = par(show p ++ "%" ++ show q)

vars :: Expr -> [String]
vars expr = sort (rmduplicates (variables expr))

variables :: Expr -> [String]
variables (Val i) = []
variables (Var x) = [x]
variables (p :+: q) = (variables p) ++ (variables q)
variables (p :-: q) = (variables p) ++ (variables q)
variables (p :*: q) = (variables p) ++ (variables q)
variables (p :/: q) = (variables p) ++ (variables q)
variables (p :%: q) = (variables p) ++ (variables q)

rmduplicates :: Eq a => [a] -> [a]
rmduplicates [] = []
rmduplicates (x:xs)
	| elem x xs = rmduplicates xs
	| otherwise = x : rmduplicates xs

evalExpr :: Expr -> Valuation -> Integer
evalExpr e vals
	| verify (vars e) (map fst vals) = calc e vals
	| otherwise = error "not all variables are assigned values"

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

valuations :: [(Name,Domain)] -> [Valuation]
valuations xs = combinePairs (formPairs xs)

formPairs :: [(Name,Domain)] -> [Valuation]
formPairs [] = []
formPairs (x:xs) = [[((fst x),j) | j<-(snd x)]] ++ formPairs (xs)


combinePairs :: [Valuation] -> [Valuation]
combinePairs [] = []
combinePairs ([]:_) = []
combinePairs (x:xs) = (combine [head x] xs) ++ combinePairs ((tail x) : xs)

combine :: Valuation -> [Valuation] -> [Valuation]
combine _ [] = []
combine v (x:xs) = (v ++ x) : combine v xs

--toExpr :: String -> Expr
--toExpr s = create (tokenize s)

create :: [String] -> Expr
create (x:xs)
	| isNumber (head x) = (Val (toNumber x)) ++ (create xs)
	| isVariable (head x) = (Var x ) ++ (create xs)
--	| x == "+" = :+: ++ (create xs)
	| otherwise = ":" ++ x ++ ":" ++ (create xs)

func :: [String] -> Expr
func xs
	| isNumber s = Val (toNumber s)
	| isVariable s = Var s
	| o == '+' = func (takeWhile notOperator s) :+: func tail (dropWhile notOperator s)
	| o == '-' = func (takeWhile notOperator s) :-: func tail (dropWhile notOperator s)
	| o == '*' = func (takeWhile notOperator s) :*: func tail (dropWhile notOperator s)
	| o == '/' = func (takeWhile notOperator s) :/: func tail (dropWhile notOperator s)
	| o == '%' = func (takeWhile notOperator s) :%: func tail (dropWhile notOperator s)
	where o = head (dropWhile notOperator s)

toNumber :: String -> Integer -> Integer
toNumber [] i = i
toNumber (x:xs) i = getNumber xs (i*10 + (toInteger (digitToInt x)))

tokenize :: String -> [String]
tokenize "" = []
tokenize s 
	| head s == ' ' = tokenize (tail s)
	| isNumber (head s) = [takeWhile isNumber s] ++ tokenize (dropWhile isNumber s)
	| isVariable (head s) = [takeWhile isVariable s] ++ tokenize (dropWhile isVariable s)
	| otherwise = [[head s]] ++ tokenize (tail s)

--isNumber :: Char -> Bool
--isNumber c = (fromEnum c < 58 && 47<fromEnum c)

isVariable :: Char -> Bool
isVariable c = (fromEnum c < 91 && 64<fromEnum c) || (fromEnum c < 123 && 96<fromEnum c)

--(Var "x") :+: (Val 2 :*: Val 3)


