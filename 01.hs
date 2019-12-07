main = fmap lines (readFile "01-input.txt") >>= putStrLn . show . processLines

processLines :: [String] -> Integer
processLines (line : rest) = computeFuel (read line) + processLines rest
processLines [] = 0

computeFuel mass = mass `div` 3 - 2