

--module Ex1to10 (myLast, myButLast, elementAt, myLength, myReverse, 
--isPalindrom, flatten, compress, pack, encode) where


-- returns last value in a list
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

-- returns the 2nd last value by removing the last then finding last of the remainding
myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast [x] = error "only a single element"
myButLast xs = myLast ys
            where 
                ys = removeLast xs
                removeLast :: [a] -> [a]
                removeLast [x] = []
                removeLast (x:xs) = [x] ++ removeLast xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty set"
elementAt xs n = elementLocation xs n 1
                where
                    elementLocation :: [a] -> Int -> Int -> a
                    elementLocation [] _ _ = error "value out of bounds"
                    elementLocation (x:xs) n p  | n == p = x
                                                | otherwise = elementLocation xs n (p + 1)
                            

myLength :: [a] -> Int 
myLength [] = 0
myLength xs = listLength xs 0
                where
                    listLength :: [a] -> Int -> Int
                    listLength [] n = n
                    listLength (x:xs) n = listLength xs (n + 1)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome xs | as == bs = True
                | otherwise = False
                    where 
                        (as, bs) = splitList xs []

                        splitList :: [a] -> [a] -> ([a],[a])
                        splitList (l:ls) gs | myLength gs == myLength ([l] ++ ls) = (gs,[l] ++ ls)
                                            | myLength gs > myLength ls = error "adding list greater than subtracting list"
                                            | otherwise = splitList ls $ gs ++ [l]