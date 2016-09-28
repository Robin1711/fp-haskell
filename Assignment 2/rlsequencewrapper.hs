--selfrle :: [Int]
---- Insert your own code here.
--selfrle = create [1] [1]
--	where 
--		create :: [Int] -> [Int] -> [Int]
--	--	create (x:xs) (y:ys) = (run last(x:xs) y) : (create [([x]++xs)] [(ys ++ (run (op (x:xs)) y))])
--		create xs ys = (run (op xs) (head ys)) ++ (create xs ((tail ys) ++ (run (op xs) (head ys))))

--xs	add 1 - add (op last xs) - add 
--ys	add 1 - remove fst ys - add op last xs

create :: [Int] -> [Int] -> [Int]
create xs (y:ys) = head xs : create ((tail xs) ++ toAdd (last xs) y) (ys ++ toAdd (last xs) y)
--create xs (y:ys) = xs : create (toAdd (last xs) y) (ys ++ toAdd (last xs) y)

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

-- Do not change the fololowing wrapper code
--wrapper :: String -> [Int]
--wrapper input = take (read input::Int) selfrle

--main =  print . wrapper =<< getLine

{-development xs: ??
development ys: ??

1 : create [2] [2]
1 : 2 : create [1,1] [1,1]
1 : 2 : 1 : create [1,2] [1,2]
1 : 2 : 1 : 1 : create 2 ++ toAdd 2 1


22 11 2 1 22 1 2
2 2 1 1 2 1

add to xs		what to add
	same ys		fst ys, last xs
-}