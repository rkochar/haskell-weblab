module Week5 where

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

sqroot :: Double -> Double
sqroot n = loop approximations
  	where
    	next a = (a+n/a)/2
    	approximations = iterate next 1.0
    	loop (x:y:ys)
      		| abs (x-y) < 0.00001 = y
      		| otherwise           = loop (y:ys)
