> module DataStructures where 

> import qualified Data.Array.IArray as A
> import Data.Array.IArray (Array, listArray, (!), bounds, elems, ixmap)

> type Queue a = [a]

> emptyQueue :: Queue a
> emptyQueue = []

> isEmpty :: Queue a -> Bool
> isEmpty [] = True
> isEmpty _  = False

> enqueue :: a -> Queue a -> Queue a
> enqueue x queue = queue ++ [x]

view :: Queue a -> Maybe a
view []    = Nothing
view (x:_) = Just x

> dequeue :: Queue a -> (Maybe a, Queue a)
> dequeue []     = (Nothing, [])
> dequeue (x:xs) = (Just x, xs)


> rotateLeftTillZero :: Array Int Int -> Array Int Int
> rotateLeftTillZero nums = 
>   let (start, end) = bounds nums
>       zeroIndex = head [i | i <- [start..end], nums!i == 0]
>       shiftedIndex i = (i + zeroIndex) `mod` (end + 1)
>   in ixmap (start, end) shiftedIndex nums


Below is an implementation of Deque (Doubled-ended Queue)

> type Deque a = ([a], [a]) deriving Show 

> initDeque :: Deque a
> initDeque = ([], [])

ghci> let deque = initDeque :: Deque Int
ghci> let deque1 = pushFront 1 deque
ghci> isEmptyDeque deque
True
ghci> isEmptyDeque deque1
False

> isEmptyDeque :: Deque a -> Bool
> isEmptyDeque ([], []) = True
> isEmptyDeque _        = False

> pushFront :: a -> Deque a -> Deque a
> pushFront x (a, z) = (x : a, z)

> pushBack :: a -> Deque a -> Deque a
> pushBack x (a, z) = (a, x : z)

> viewFront :: Deque a -> Maybe a
> viewFront ([], [])     = Nothing
> viewFront (x:_, _)     = Just x
> viewFront ([], z)      = Just (last z)

ghci> viewFront deque1
Just 1
ghci> popFront deque1
(Just 1,([],[]))

> peekBack :: Deque a -> Maybe a
> peekBack ([], [])     = Nothing
> peekBack (_, x:_)     = Just x
> peekBack (a, [])      = Just (head a)

> popFront :: Deque a -> (Maybe a, Deque a)
> popFront ([], [])     = (Nothing, initDeque)
> popFront (x:xs, z)    = (Just x, (xs, z))
> popFront ([], z)      = popFront (reverse z, [])  -- Rebalance

> popBack :: Deque a -> (Maybe a, Deque a)
> popBack ([], [])      = (Nothing, initDeque)
> popBack (a, x:xs)     = (Just x, (a, xs))
> popBack (a, [])       = popBack ([], reverse a)  -- Rebalance

Below is the implementation of Circular Buffer 

> type CircularBuffer a = ([a], Int, [a])

> circBuff :: Int -> CircularBuffer a
> circBuff n = (take n $ repeat (error "empty slot"), 0, [])


> readBuffer :: CircularBuffer a -> Int -> a
> readBuffer (a, _, z) i = (a ++ reverse z) !! i

> writeBuffer :: a -> CircularBuffer a -> CircularBuffer a
> writeBuffer x (a, i, z) =
>   let (a', _) = splitAt i a
>       z' = take (i - 1) z
>   in (a' ++ [x], (i + 1) `mod` length a, z')