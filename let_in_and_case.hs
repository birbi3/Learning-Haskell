calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
  where what [] = "empty"
        what [x] = "a singleton list."
        what xs = "a longer list."
