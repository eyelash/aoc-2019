import Data.Text (pack, strip, splitOn, unpack)
import Data.Array.Unboxed
import Data.Array.IO

main = do
  input <- readFile "02-input.txt"
  p <- thaw (parseInput input)
  restore (12, 2) p
  result <- runProgram 0 p
  putStrLn (show result)

parseInput :: String -> UArray Int Int
parseInput = createArray. map (read . unpack) . splitOn (pack ",") . strip . pack

createArray :: [Int] -> UArray Int Int
createArray l = listArray (0, length l - 1) l

restore :: (Int, Int) -> IOUArray Int Int -> IO ()
restore (noun, verb) p = writeArray p 1 noun >> writeArray p 2 verb

getOp :: Int -> Int -> Int -> Int
getOp 1 = (+)
getOp 2 = (*)

runProgram :: Int -> IOUArray Int Int -> IO Int
runProgram i p = do
  opcode <- readArray p i
  if opcode == 99
    then
      readArray p 0
    else do
      arg1 <- readArray p (i + 1) >>= readArray p
      arg2 <- readArray p (i + 2) >>= readArray p
      dst <- readArray p (i + 3)
      writeArray p dst (getOp opcode arg1 arg2)
      runProgram (i + 4) p
