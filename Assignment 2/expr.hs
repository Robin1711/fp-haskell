type Name = String
type Domain = [Integer]

data Expr = Val Integer
	| Var Name
	| Expr :+: Expr
	| Expr :-: Expr
	| Expr :*: Expr
	| Expr :/: Expr
	| Expr :%: Expr