import Data.Text (pack, strip, splitOn, unpack)
import Data.Array.Unboxed
import Data.Array.IO
import Control.Monad (when, forM_)

main = readFile "02-input.txt" >>= findInputs . parseInput

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

findInputs :: UArray Int Int -> IO ()
findInputs p' =
  forM_ [0..99] $ \noun ->
    forM_ [0..99] $ \verb -> do
      p <- thaw p'
      restore (noun, verb) p
      result <- runProgram 0 p
      when (result == 19690720) $ do
        putStrLn ("noun = " ++ show noun)
        putStrLn ("verb = " ++ show verb)
        putStrLn ("100 * noun + verb = " ++ show (100 * noun + verb))
