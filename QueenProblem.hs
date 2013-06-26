import System.Environment
import Control.Monad.Logic
import Control.Monad.Logic.Class

{-
    This code is inspired by the prolog solution at
    http://www.javaist.com/blog/2008/11/06/eight-queens-problem-in-prolog/
-}

template :: Int -> Logic [(Int, Int)]
template n = do
    let xs = repeat $ choices [1..n]
        ys = map return [1..n]
    sequence $ zipWith combine xs ys
    where
        choices = msum . map return
        combine :: Monad m => m a -> m b -> m (a, b)
        combine mx my = do
            x <- mx
            y <- my
            return (x, y)

solution :: [(Int, Int)] -> Logic [(Int, Int)]
solution []              = return []
solution ((x,y) : other) = do
    guard $ noAttack (x, y) other
    sol <- solution other
    return $ (x,y) : sol

noAttack _       []                = True
noAttack (x1,y1) ((x2,y2) : other) =
    x1 /= x2
    &&
    y2 - y1 /= x2 - x1
    &&
    y2 - y1 /= x1 - x2
    &&
    noAttack (x1, y1) other

main = do
    args <- getArgs
    let size = read (head args) :: Int

    putStrLn $ unlines $ map show $ observeAll $ template size >>= solution
