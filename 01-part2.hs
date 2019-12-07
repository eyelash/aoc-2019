main = fmap lines (readFile "01-input.txt") >>= putStrLn . show . processLines

processLines :: [String] -> Integer
processLines (line : rest) = computeFuel (read line) + processLines rest
processLines [] = 0

computeFuel mass = let fuel = mass `div` 3 - 2 in
                   if fuel <= 0 then 0 else fuel + computeFuel fuel
