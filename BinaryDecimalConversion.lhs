> module BinaryDecimalConversion where 
> import Data.Char

> type Binary = Int 
> binaryToDecimal :: [Binary] -> Int 
> binaryToDecimal b = sum [b' * w | (b',w) <- zip b (iterate (*2) 1 ) ]

ghci> binaryToDecimal [1,1,1]
7

> binaryToDecimal' :: [Binary] -> Int
> binaryToDecimal' = foldr(\x y -> x + 2*y) 0 

ghci> binaryToDecimal' [1,1,1]
7

> decimalToBinary :: Int -> [Binary]
> decimalToBinary 0 = []
> decimalToBinary x = mod x 2 : decimalToBinary (div x 2)

ghci> decimalToBinary (binaryToDecimal [1,1,1])
[1,1,1]
ghci> decimalToBinary 13
[1,0,1,1]

> make8 :: [Binary] -> [Binary]
> make8 bs = take 8 (bs ++ repeat 0)

> encode :: String -> [Binary]
> encode = concat . map (make8 . decimalToBinary . ord)
