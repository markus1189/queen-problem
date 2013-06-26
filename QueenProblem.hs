import System.Environment
import Control.Applicative
import Control.Monad.Logic

{-
    This code is inspired by the prolog solution at
    http://www.javaist.com/blog/2008/11/06/eight-queens-problem-in-prolog/
-}

template :: Int -> Logic [(Int, Int)]
template n = do
    let xs = repeat $ choices [1..n]
        ys = map return [1..n]
    zipWithM combine xs ys
    where
        choices = msum . map return
        combine :: Applicative m => m a -> m b -> m (a, b)
        combine  = liftA2 (,)

solution :: [(Int, Int)] -> Logic [(Int, Int)]
solution []              = return []
solution (queen : other) = do
    guard $ queen `noAttack` other
    otherSolutions <- solution other
    return $ queen : otherSolutions

noAttack :: (Int,Int) -> [(Int,Int)] -> Bool
noAttack _       []                = True
noAttack (x1,y1) ((x2,y2) : other) = x1 /= x2
                                  && abs (y2 - y1) /= abs (x2 - x1)
                                  && noAttack (x1,y1) other

main :: IO ()
main = do
    size <- fmap (read . head) getArgs
    putStrLn . unlines . map show . observeAll $ template size >>= solution
