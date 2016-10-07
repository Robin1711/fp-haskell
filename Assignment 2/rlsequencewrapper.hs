selfrle :: [Int]
-- Insert your own code here.
selfrle =  1 : 2 : create [2] [2]
	where 
		create :: [Int] -> [Int] -> [Int]
		create xs (y:ys) = head xs : create ((tail xs) ++ toAdd lst y) (ys ++ toAdd lst y)
			where lst = last xs

toAdd :: Int -> Int -> [Int]
toAdd n l = run (op n) l

op :: Int -> Int
op n
	| n == 1 = 2
	| otherwise = 1

run :: Int -> Int -> [Int]
run n l
	| l == 1 = [n]
	| otherwise = [n,n]

 --Do not change the fololowing wrapper code
wrapper :: String -> [Int]
wrapper input = take (read input::Int) selfrle

main =  print . wrapper =<< getLine

