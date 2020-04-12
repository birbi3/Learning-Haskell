
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v) : xs) = if key == k
                                  then Just v
                                  else findKey key xs

findKeyTwo ::  (Eq k) => k -> [(k,v)] -> Maybe v
findKeyTwo key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
