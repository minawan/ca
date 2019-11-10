import Control.Monad.Extra (iterateMaybeM)
import Data.Bits (shiftL, testBit, (.|.))
import System.Environment (getArgs)

type Rule = Bool -> Bool -> Bool -> Bool

zeroCell = '⬛'
oneCell = '⬜'

getNext :: Rule -> [Bool] -> [Bool]
getNext rule [] = getNext' rule False False [False]
getNext rule (left:cells) =
  rule False False left : getNext' rule False left cells

getNext' :: Rule -> Bool -> Bool -> [Bool] -> [Bool]
getNext' rule left mid [] = [rule left mid False, rule mid False False]
getNext' rule left mid (right:cells) =
  rule left mid right : getNext' rule mid right cells

genRule :: Int -> Rule
genRule n left mid right = testBit n repr
  where toBit cell = if cell then 1 else 0
        repr = foldl (\x y -> shiftL x 1 .|. y) 0 (map toBit [left, mid, right])

showPadded :: Int -> [Bool] -> String
showPadded n cells
  | n <= 0 = ""
  | otherwise = padding ++ map showCell cells ++ padding
  where showCell x = if x then oneCell else zeroCell
        padding = take ((n - length cells) `div` 2) $ repeat zeroCell

printTrace :: Rule -> Int -> [Bool] -> Int -> IO ()
printTrace rule width cells maxIter = do
  _ <- iterateMaybeM (aux rule width) ([True], maxIter)
  return ()

aux :: Rule -> Int -> ([Bool], Int) -> IO (Maybe ([Bool] , Int))
aux rule width (cells, i)
  | i <= 0 = return Nothing
  | otherwise = case getNext rule cells of
                  [] -> return Nothing
                  cells -> do putStrLn $ showPadded width cells
                              return $ Just (cells, i - 1)

main :: IO ()
main = do
  args <- getArgs
  let rule = genRule (read (args !! 0) :: Int)
  let maxIter = read (args !! 1) :: Int
  let width = 2 * maxIter + 1
  let initialCells = [True]
  putStrLn $ showPadded width initialCells
  printTrace rule width initialCells maxIter
