capital :: String -> String
capital "" = "Empty String, whoops!"
capital all@(x:xs) = "The first string letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "Skinny"
  | bmi <= 25.0 = "Normal"
  | bmi <= 30.0 = "Large"
  | otherwise = "Large and in charge"

bmiTellBetter :: (RealFloat a) => a -> a -> String
bmiTellBetter weight height
  | weight / height ^ 2 <= 18.5 = "Skinny"
  | weight / height ^ 2 <= 25.0 = "Normal"
  | weight / height ^ 2 <= 30.0 = "Large"
  | otherwise = "Large and in charge"

myMax :: (Ord a) => a -> a -> a
myMax a b | a > b = a | otherwise = b

bmiTellWhere :: (RealFloat a) => a -> a -> String
bmiTellWhere weight height
  | bmi <= skinny = "skinny"
  | bmi <= normal = "normal"
  | bmi <= fat = "large"
  | otherwise = "Large and in charge"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstname lastname = [ f ] ++ " . " ++ [ l ] ++ " . "
  where (f : _) = firstname
        (l : _) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2
