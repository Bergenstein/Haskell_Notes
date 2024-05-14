> module HigherOrderFunctions where 

A higher order function is one that takes a function as an input and/or returns a function as an output. A curried function is a function that returns a function as an output 

> map' :: (a -> b) -> [a] -> [b]
> map' f xs = [f x | x <- xs]

> map'' :: Functor f => (a -> b) -> f a -> f b 
> map'' f' fa = fmap f' fa

ghci> map'' (^2) [1,2,3]
[1,4,9]
ghci> map'' (^3) (Just 3)
Just 27
ghci> map'' (^2) (Left 4)
Left 4
ghci> map'' (^2) (Right 4)
Right 16

map is a polymorphic function that can be applied to functors of any type. 

> map_rec :: (a -> b) -> [a] -> [b]
> map_rec f [] = []
> map_rec f (x:xs) = f x : map_rec f xs

Mapping over a tree using recursion:

> data Tree a = Leaf | Node a (Tree a) (Tree a) 
>   deriving (Show, Functor)

> map_rec' :: (a -> b) -> Tree a -> Tree b 
> map_rec' _ Leaf = Leaf 
> map_rec' f (Node x l r) = Node (f x) (map_rec' f l) (map_rec' f r)  

> predicate' :: (a -> Bool) -> [a] -> [a]
> predicate' p xs = [x | x <- xs, p x]

> predicate'' :: (a -> Bool) -> [a] -> [a]
> predicate'' p [] = []
> predicate'' p (x:xs) | p x       = x : predicate'' p xs
>                      | otherwise = predicate'' p xs

ghci> predicate' (>3) [1,2,3,4,5]
[4,5]

ghci> predicate'' (>3) [1,2,3,4,5]
[4,5]

> predic' :: (a -> Bool) -> [a] -> [a]
> predic' p []               = []
> predic' p xs | p (head xs) = (head xs) : predic' p (tail xs) 
>              | otherwise   = predic' p (tail xs) 

ghci> predic' (>3) [1,2,3,4,5]
[4,5]

> sums_of_even_squared :: [Int] -> Int 
> sums_of_even_squared xs = sum (map_rec (^2) (predic' even xs))

ghci> sums_of_even_squared [1,2,3,4,5,6]
56

The FoldR Function (Fold to the right): foldr takes the empty list to some value v. It applies the function f to the head of the nonempty list and then recursively processes the remainder of the list by folding them and applying the function f to them. In simpler language, replacing the cons operator with function f and replacing the empty list with value v 

The FoldL Function (Fold to the left): foldr takes an empty list to some value v. Foldl takes a nonempty list to the result of recursively processing the list using a new accumulator value obtained by applying an operator # to the current value and to the head of the list.

foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr f v [] = v
foldr f v (x:xs) = f x (foldr f v xs)

f [] = v 
f (x:xs) = x # f xs

This is foldr 

f v [] = v 
f v (x:xs) = f (v # x) xs

and this is foldl 

Examples of foldr 

sum []     = 0 -- f [] = v 
sum (x:xs) = x + sum xs -- f (x:xs) = x # f xs

sum :: Num a => [a] -> a 
sum = foldr (+) 0 -- operator # and base v  

or []     = False -- f [] = v 
or (x:xs) = x || or xs -- f (x:xs) = x # f xs 

or :: [Bool] -> Bool 
or = foldr (||) False -- operator # base v 

and []     = True 
and (x:xs) = x && and xs -- f (x:xs) = x # f xs 

and :: [Bool] -> Bool 
and = foldr (&&) True 

foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr f v []     = v 
foldr f v (x:xs) = f x (foldr f v xs) 

foldl :: (a -> b -> ) -> a -> [b] -> a 
foldl f v []     = v
foldl f v (x:xs) = foldl f (f v x) xs   

foldr takes the empty list to some value v. It applies the function f to the head of the nonempty list and then recursively processes the remainder of the list by folding them and applying the function f to them. For example: 

foldr (+) 0 [] = 0 
foldr (+) 0 (x:xs) = (+) x (foldr (+) 0 xs) 

> length' :: [a] -> Int 
> length' [] = 0 
> length' (x:xs) = 1 + length' xs 

> length'' :: [a] -> Int 
> length'' = foldr (\f x -> 1+x) 0 -- f is just some function and can be replaced with _ 

ghci> length'' [2,3,4]
3

> idlist :: [a] -> [a]
> idlist []     = []
> idlist (x:xs) = x : idlist xs  

> reverse' :: [a] -> [a] 
> reverse' []     = []
> reverse' (x:xs) = reverse' xs ++ [x]

> reverse'' :: [a] -> [a]
> reverse'' = foldr helper [] where 
>    helper x xs = xs ++ [x] 

ghci> reverse'' [1,2,3]
[3,2,1]

The FoldL Function (Fold to the left): foldr takes an empty list to some value v. Foldl takes a nonempty list to the result of recursively processing the list using a new accumulator value obtained by applying an operator # to the current value and to the head of the list.

> sum'' ::  Num a => [a] -> Int 
> sum'' []     = 0 
> sum'' (x:xs) = 1 + sum'' xs 

The code below is wrong because Functors don't fold. List is a functor that folds, but tree doesn't fold, neither does Either or Maybe. 

countfoldr :: (Num a, Functor f) => f a -> Int
countfoldr = foldr (\f n -> 1 + n) 0

Alternatively: This is correct because it uses a foldable type

> countfoldr :: (Num a, Foldable t) => t a -> a 
> countfoldr = foldr (\f n -> 1 + n) 0

> sumfoldr :: (Num a, Foldable t) => t a -> a 
> sumfoldr = foldr (+) 0

ghci> sumfoldr [1, 2.5, 3]
6.5
ghci> countfoldr [1, 3, 4.5, 6.2, -3]
5.0
ghci> sumfoldr [1, 3, 4.5, 6.2, -3]
11.7

> prodfoldr :: (Num a, Foldable t) => t a -> a 
> prodfoldr = foldr (*) 1

Function Composition: 

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id 

id :: a -> a
if = \x -> x 


