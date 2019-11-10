import Data.Bits (testBit)
import Control.Monad (foldM)
import Control.Monad.Extra (iterateMaybeM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)

type Rule = Map (Bool, Bool, Bool) Bool

zeroCell = '⬛'
oneCell = '⬜'

getNext :: Rule -> [Bool] -> Maybe [Bool]
getNext rule [] = getNext' rule False [False]
getNext rule (left:cells) =
  case Map.lookup (False, False, left) rule of
    Just left' -> case getNext'' rule False left cells of
                    Just cells' -> Just (left' : cells')
                    Nothing -> Nothing
    Nothing -> Nothing

getNext' :: Rule -> Bool -> [Bool] -> Maybe [Bool]
getNext' rule left [] =
  case Map.lookup (left, False, False) rule of
    Just cell -> Just [cell]
    Nothing -> Nothing
getNext' rule left (mid:cells) =
  case Map.lookup (False, left, mid) rule of
    Just left' -> case getNext'' rule left mid cells of
                    Just cells' -> Just (left' : cells')
                    Nothing -> Nothing
    Nothing -> Nothing

getNext'' :: Rule -> Bool -> Bool -> [Bool] -> Maybe [Bool]
getNext'' rule left mid [] =
  case Map.lookup (left, mid, False) rule of
    Just mid' -> case Map.lookup (mid, False, False) rule of
                   Just right -> Just [mid', right]
                   Nothing -> Nothing
    Nothing -> Nothing
getNext'' rule left mid (right:cells) =
  case Map.lookup (left, mid, right) rule of
    Just mid' -> case getNext'' rule mid right cells of
                   Just cells' -> Just (mid' : cells')
                   Nothing -> Nothing
    Nothing -> Nothing

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

aux :: Rule -> Int -> ([Bool], Int) -> IO (Maybe ([Bool] , Int))
aux rule width (cells, i)
  | i <= 0 = return Nothing
  | otherwise = case getNext rule cells of
                  Just cells -> (do putStrLn $ showPadded width cells
                                    return $ Just (cells, i - 1))
                  Nothing -> return Nothing

main :: IO ()
main = do
  args <- getArgs
  let rule = genRule (read (args !! 0) :: Int)
  let maxIter = read (args !! 1) :: Int
  let width = 2 * maxIter + 1
  putStrLn $ showPadded width [True]
  _ <- iterateMaybeM (aux rule width) ([True], maxIter)
  return ()
