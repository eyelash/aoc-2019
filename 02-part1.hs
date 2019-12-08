main = readFile "02-input.txt" >>= putStrLn . show . runProgram 0 . restore (12, 2) . map read . split

valid :: Char -> Bool
valid c = c >= '0' && c <= '9'

split :: String -> [String]
split [] = []
split s =
  let
    s'     = dropWhile (not . valid) s
    first  = takeWhile valid s'
    second = dropWhile valid s'
  in
    if first == [] then [] else first : split second

setValue :: a -> Int -> [a] -> [a]
setValue value 0 (x:xs) = value : xs
setValue value i (x:xs) = x : setValue value (i - 1) xs

restore :: (Int, Int) -> [Int] -> [Int]
restore (noun, verb) p = setValue verb 2 (setValue noun 1 p)

getOp :: Int -> Int -> Int -> Int
getOp 1 = (+)
getOp 2 = (*)

runProgram :: Int -> [Int] -> Int
runProgram i p =
  let
    opcode = p !! i
  in
    if opcode == 99 then
      p !! 0
    else
      let
        arg1 = p !! (p !! (i + 1))
        arg2 = p !! (p !! (i + 2))
        value = getOp opcode arg1 arg2
        dst = p !! (i + 3)
      in
        runProgram (i + 4) (setValue value dst p)
