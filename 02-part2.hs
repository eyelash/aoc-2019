import Data.Text (pack, strip, splitOn, unpack)
import Data.Vector.Unboxed (Vector, fromList, thaw)
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable (read, write)
import Control.Monad (when, forM_)

readVector = Data.Vector.Unboxed.Mutable.read
writeVector = Data.Vector.Unboxed.Mutable.write

main = readFile "02-input.txt" >>= findInputs . parseInput

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

findInputs :: Vector Int -> IO ()
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
