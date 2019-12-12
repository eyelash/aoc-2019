import Data.Text (pack, strip, splitOn, unpack)

main = readFile "02-input.txt" >>= putStrLn . show . runProgram 0 . restore (12, 2) . map (read . unpack) . splitOn (pack ",") . strip . pack

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
