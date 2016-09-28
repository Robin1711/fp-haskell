primes :: [Integer]
-- Insert your own code here.





-- Do not change the following wrapper code
wrapper :: String -> [Integer]
wrapper input = take (read input::Int) primes

main =  print . wrapper =<< getLine
