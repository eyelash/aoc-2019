import Data.Text (pack, strip, splitOn, unpack)
import Data.Vector.Unboxed (Vector, fromList, thaw)
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable (read, write)

readVector = Data.Vector.Unboxed.Mutable.read
writeVector = Data.Vector.Unboxed.Mutable.write

main = do
  input <- readFile "02-input.txt"
  p <- thaw (parseInput input)
  restore (12, 2) p
  result <- runProgram 0 p
  putStrLn (show result)

parseInput :: String -> Vector Int
parseInput = fromList . map (read . unpack) . splitOn (pack ",") . strip . pack

restore :: (Int, Int) -> IOVector Int -> IO ()
restore (noun, verb) p = writeVector p 1 noun >> writeVector p 2 verb

getOp :: Int -> Int -> Int -> Int
getOp 1 = (+)
getOp 2 = (*)

runProgram :: Int -> IOVector Int -> IO Int
runProgram i p = do
  opcode <- readVector p i
  if opcode == 99
    then
      readVector p 0
    else do
      arg1 <- readVector p (i + 1) >>= readVector p
      arg2 <- readVector p (i + 2) >>= readVector p
      dst <- readVector p (i + 3)
      writeVector p dst (getOp opcode arg1 arg2)
      runProgram (i + 4) p
