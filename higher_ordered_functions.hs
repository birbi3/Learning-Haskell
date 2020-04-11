compareWithHundred :: ( Num a, Ord a ) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

useZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
useZipWith _ [] _ = []
useZipWith _ _ [] = []
useZipWith f (x:xs) (y:ys) = f x y : useZipWith f xs ys

useFlip :: (a -> b -> c) -> (b -> a -> c)
useFlip f = g
        where g x y = f y x

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) =
  let smallerSorted = quickSort (filter (<= x) xs)
      biggerSorted = quickSort (filter (>x) xs)
  in smallerSorted ++ [x] ++ biggerSorted


largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000 ,99999..])
  where p x = x `mod` 3892 == 0

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
