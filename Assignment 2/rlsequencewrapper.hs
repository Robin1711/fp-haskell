selfrle :: [Int]
-- Insert your own code here.
selfrle =  1 : 2 : create [2] [2]
	where 
		create :: [Int] -> [Int] -> [Int]
		create xs (y:ys) = head xs : create ((tail xs) ++ toAdd (last xs) y) (ys ++ toAdd (last xs) y)

toAdd :: Int -> Int -> [Int]
toAdd n l = run (op n) l

op :: Int -> Int
op n
	| n == 1 = 2
	| otherwise = 1

run :: Int -> Int -> [Int]
run n l
	| l == 1 = [n]
	| otherwise = [n] ++ run n (l-1)

 --Do not change the fololowing wrapper code
wrapper :: String -> [Int]
wrapper input = take (read input::Int) selfrle

main =  print . wrapper =<< getLine
