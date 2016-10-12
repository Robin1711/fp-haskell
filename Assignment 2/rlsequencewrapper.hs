selfrle :: [Int]
-- Insert your own code here.
selfrle =  1 : 2 : create [2] [2] 2
	where 
		create :: [Int] -> [Int] -> Int -> [Int]
		create xs (y:ys) lst = head xs : create ((tail xs) ++ rep) (ys ++ rep) (last rep)
			where rep = replicate y (op lst)

op :: Int -> Int
op n
	| n == 1 = 2
	| otherwise = 1

 --Do not change the fololowing wrapper code
wrapper :: String -> [Int]
wrapper input = take (read input::Int) selfrle

main =  print . wrapper =<< getLine
