module Writer where

import Data.Monoid
  
newtype Writer w a = Writer (a, w)

tell :: w -> Writer w ()
tell x = Writer ((), x)

runWriter :: Writer w a -> (a, w)
runWriter (Writer x) = x

instance Functor (Writer w) where
  -- fmap :: (a -> b) -> Writer w a -> Writer w b
  fmap f (Writer (x, log)) = Writer (f x, log)
  
instance Monoid w => Applicative (Writer w) where
  -- pure :: a -> Writer w a
  pure x = Writer (x, mempty)
  
  -- (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
  Writer (fx, log1) <*> Writer (x, log2) = Writer (fx x, mappend log1 log2)
  
instance Monoid w => Monad (Writer w) where
  -- return :: a -> Writer w a
  return = pure
  
  -- (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
  Writer (x, log) >>= gx = let (y, log2) = runWriter (gx x)
                           in Writer (y, mappend log log2)



data Op = Add Double | Subtract Double | Multiply Double | Divide Double | Sqrt
  deriving (Show)

opCost :: Op -> Sum Int
opCost (Add _) = Sum 1
opCost (Subtract _) = Sum 2
opCost (Multiply _) = Sum 5
opCost (Divide _) = Sum 10
opCost Sqrt = Sum 20

opLog :: Op -> String
opLog (Add x) = "Adding " ++ show x
opLog (Subtract x) = "Subtracting " ++ show x
opLog (Multiply x) = "Multiplying by " ++ show x
opLog (Divide x) = "Dividing by " ++ show x
opLog Sqrt = "Taking Square Root"

applyOp :: Op -> Double -> Double
applyOp (Add x) y = x + y
applyOp (Subtract x) y = y - x
applyOp (Multiply x) y = x * y
applyOp (Divide x) y = y / x
applyOp Sqrt y = if y >= 0 then sqrt y else y

applyOpCount :: Op -> Double -> Writer (Sum Int) Double
applyOpCount op d = do
    tell (opCost op)
    return (applyOp op d)

applyOpsCount :: [Op] -> Double -> Writer (Sum Int) Double
applyOpsCount [] d = return d 
applyOpsCount (op : ops) d = do
    y <- applyOpCount op d
    applyOpsCount ops y

applyAndCountOperations :: [Op] -> Double -> (Double, Sum Int)
applyAndCountOperations ops d = runWriter $ applyOpsCount ops d

applyOpLog :: Op -> Double -> Writer [String] Double
applyOpLog op d = do
    tell [opLog op]
    return $ applyOp op d

applyOpsLog :: [Op] -> Double -> Writer [String] Double
applyOpsLog [] d = return d
applyOpsLog (op : ops) d = do
    dd <- applyOpLog op d
    applyOpsLog ops dd
    
applyAndLogOperations :: [Op] -> Double -> (Double, [String])
applyAndLogOperations ops d = runWriter $ applyOpsLog ops d
  
