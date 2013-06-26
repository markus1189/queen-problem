import System.Environment
import Control.Monad (guard)

type Queen = (Int,Int)
type Board = [Queen]

queens :: Int -> [Board]
queens n = placeQueens n n

placeQueens :: Int -> Int -> [Board]
placeQueens _ 0 = [[]]
placeQueens size k = do
  queens' <- placeQueens size (k-1)
  col <- [1..size]
  let queen = (k,col)
  guard $ queen `safeOn` queens'
  return (queen:queens')

safeOn :: Queen -> Board -> Bool
safeOn q = not . any (inCheck q)

inCheck :: Queen -> Queen -> Bool
inCheck p1@(x1,y1) p2@(x2,y2) = x1 == x2 || y1 == y2 || p1 `isDiagonalTo` p2

isDiagonalTo :: Queen -> Queen -> Bool
isDiagonalTo (x1,y1) (x2,y2) = abs (x1 - x2) == abs (y1 - y2)

main :: IO ()
main = do
  size <- fmap (read . head) getArgs
  putStrLn . unlines . map show . queens $ size
