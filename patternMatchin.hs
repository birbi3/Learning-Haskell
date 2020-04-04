charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a , b , c ) -> b
second (_ , y , _ ) = y

third :: (a , b , c ) -> c
third (_ , _ , z ) = z

tell :: (Show a) => [a] -> String
tell [] = "This list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long The first two elements are : " ++ show x ++ " and " ++ show y

leng :: (Num b) => [a] -> b
leng [] = 0
leng (_:xs) = 1 + leng xs
