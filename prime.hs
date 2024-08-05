-- numnber theory functionz
-- 4 fun and 4 profit
-- (the profit is knowledge)
-- learn 2 create     learn 2 destroy
-- click click click click click click click click click click click click

divides :: Integral a => a -> a -> Bool
divides a b = b `mod` a == 0

prime :: Integral a => a -> Bool
prime n = not $ any (`divides` n) [2..iSqrtN]
          where iSqrtN = floor $ sqrt $ fromIntegral n 

divisors :: Integral a => a -> [a]
divisors n = [x | x <- [1..n], x `divides` n]

isHighlyComposite :: Integral a => a -> Bool
isHighlyComposite n = all (lessDivisorsThan n) [1..n-1]
                      where numDivisors = length . divisors
                            -- return true if z has less divisors than x
                            lessDivisorsThan x z = (numDivisors z) < (numDivisors x)

highlyCompositeNumbers :: Integral a => [a]
highlyCompositeNumbers = filter isHighlyComposite [1..] 

--primeCounting :: (Num a, Enum a) => a -> Int
--primeCounting n = length $ filter prime [2..n]


-- numbers n where the amount of divisors divides the number
-- (e.g. 12 has 6 divisors, and 6 divides 12)
refactorableNumbers :: [Int]
refactorableNumbers = filter isHoly [1..]
              where isHoly n = (length $ divisors n) `divides` n

main = do
       print $ take 14 highlyCompositeNumbers 
