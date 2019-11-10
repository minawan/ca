import Control.Monad (foldM)
import Control.Monad.Extra (iterateMaybeM)
import Data.Bits (testBit)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)

type Rule = Map (Bool, Bool, Bool) Bool

zeroCell = '⬛'
oneCell = '⬜'

getNext :: Rule -> [Bool] -> [Bool]
getNext rule [] = getNext' rule False [False]
getNext rule (left:cells) =
  case Map.lookup (False, False, left) rule of
    Just left' -> case getNext'' rule False left cells of
                    [] -> []
                    cells' -> left' : cells'
    Nothing -> []

getNext' :: Rule -> Bool -> [Bool] -> [Bool]
getNext' rule left [] =
  case Map.lookup (left, False, False) rule of
    Just cell -> [cell]
    Nothing -> []
getNext' rule left (mid:cells) =
  case Map.lookup (False, left, mid) rule of
    Just left' -> case getNext'' rule left mid cells of
                    [] -> []
                    cells' -> left' : cells'
    Nothing -> []

getNext'' :: Rule -> Bool -> Bool -> [Bool] -> [Bool]
getNext'' rule left mid [] =
  case Map.lookup (left, mid, False) rule of
    Just mid' -> case Map.lookup (mid, False, False) rule of
                   Just right -> [mid', right]
                   Nothing -> []
    Nothing -> []
getNext'' rule left mid (right:cells) =
  case Map.lookup (left, mid, right) rule of
    Just mid' -> case getNext'' rule mid right cells of
                   [] -> []
                   cells' -> mid' : cells'
    Nothing -> []

genRule :: Int -> Rule
genRule n = Map.fromList . zip allBoolTriples $ map (testBit n) [0..7]
  where bools = [False, True]
        allBoolTriples = [(x, y, z) | x <- bools, y <- bools, z <- bools]

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
