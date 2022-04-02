module Boundary where

data Boundary a = MinBound | Value a | MaxBound
  deriving (Show, Eq, Ord)

data Range a = R (Boundary a) (Boundary a)
  deriving (Show, Eq)

newtype RangedSet a = RS [Range a]

example = RS [R MinBound (Value 0) , R (Value 2) (Value 5)]

-- {1,2,3,4,6,8,9,10}
myRangedSet :: RangedSet Int
myRangedSet = RS [R (Value 1) (Value 5), R (Value 6) (Value 7), R (Value 8) (Value 11)]

member :: Ord a => a -> RangedSet a -> Bool
member x (RS [R MinBound MaxBound]) = Value x < MaxBound
member _ (RS [])                    = False
member x (RS (r : rs)) = case r of R MinBound (Value v)    -> if x < v
                                                              then True
                                                              else member x (RS rs)
                                   R (Value v) MaxBound    -> if x >= v && Value v < MaxBound
                                                              then True
                                                              else False
                                   R (Value v1) (Value v2) -> if x >= v1 && x < v2
                                                              then True
                                                              else member x (RS rs)
                                   _                       -> False

isValidRules :: Ord a => Range a -> Bool
isValidRules rs = case rs of (R MinBound MaxBound)     -> True
                             (R MinBound (Value _))    -> True
                             (R (Value _) MaxBound)    -> True
                             (R (Value v1) (Value v2)) -> v1 < v2
                             _                         -> False

isValidHelper :: Ord a => [Range a] -> Bool
isValidHelper []                       = True
isValidHelper [_]                      = True
isValidHelper (_ : (R MinBound _) : _) = False
isValidHelper ((R _ MaxBound) : _)     = False
isValidHelper ((R _ (Value v1)) : r2@(R (Value v2) _) : rs) = v1 < v2 && isValidHelper (r2 : rs)
isValidHelper _                        = False

isValid :: Ord a => RangedSet a -> Bool
isValid (RS []) = True
isValid (RS rs) = all isValidRules rs && isValidHelper rs

--isValid :: Ord a => RangedSet a -> Bool
--isValid (RS []) = True
--isValid (RS [R x y]) = x <= y
--isValid (RS (R x y : R z w : rs)) = x <= y && y < z && isValid (RS (R z w : rs))

insertRange :: Ord a => Range a -> RangedSet a -> RangedSet a
insertRange r (RS [])                = RS [r]
insertRange r@(R MinBound _) (RS rs) = RS $ r : rs
insertRange r@(R _ MaxBound) (RS rs) = RS $ rs ++ [r]
insertRange r (RS a@[R MinBound _]) = RS $ a ++ [r]
insertRange r (RS (a@(R MinBound _) : rs)) = RS $ a : rest 
                                             where (RS rest) = insertRange r (RS rs)
insertRange r (RS a@[R _ MaxBound]) = RS $ r : a
insertRange x@(R _ (Value v2)) (RS rss@(r@(R (Value vv1) _) : rs)) = if v2 < vv1
                                                                     then RS $ x : rss
                                                                     else RS $ r : rest
insertRange r (RS rs) = RS $ r : rs                                                                          where (RS rest) = insertRange x (RS rs)
                                                                          
instance Ord a => Semigroup (RangedSet a) where
--  	(<>) (RS rs1) (RS rs2)  = RS $ rs1 ++ rs2
	(<>) (RS []) r = r
	(<>) (RS (rs : rs1)) rs2 = (<>) (RS rs1) (insertRange rs rs2)
  
instance Ord a => Monoid (RangedSet a) where
  	mempty = RS []
  	mappend = (<>)
  	
instance Functor Boundary where
  	fmap f MinBound = MinBound
  	fmap f MaxBound = MaxBound
  	fmap f (Value v) = Value $ f v
  	
instance Functor Range where
  	fmap f (R r1 r2) = R (fmap f r1) (fmap f r2)  	

instance Functor RangedSet where
  	fmap f (RS rs) = RS $ map (fmap f) rs