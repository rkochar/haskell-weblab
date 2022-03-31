module Week3a where

import Data.List (nub, intercalate)
import Data.Monoid

data Person = Adult String String Int Occupation
            | Child String Int Int

data Occupation = Engineer
                | Lawyer

giveFullName :: Person -> String
giveFullName (Adult f s _ _) = f ++ " " ++ s
giveFullName (Child f _ _) = f

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
  	deriving (Show, Eq)

occurs :: Ord a => a -> Tree a -> Bool
occurs _ Empty = False
occurs element (Leaf l) = element == l
occurs element (Node left root right) = if element == root
                                                      then True
                                                      else if element < root
                                                           then occurs element left
                                                           else occurs element right

numberOfElementsInTree :: Tree a -> Int
numberOfElementsInTree tree = case tree of
	Empty             -> 0
	Leaf _            -> 1
	Node left _ right -> 1 + numberOfElementsInTree left + numberOfElementsInTree right

is_balanced :: Tree a -> Bool
is_balanced Empty = True
is_balanced (Leaf _) = True
is_balanced (Node left root right) = leftSum + 1 == rightSum || rightSum + 1 == leftSum
									 	where leftSum  = numberOfElementsInTree left
									 	      rightSum = numberOfElementsInTree right

flatten :: Tree a -> [a]
flatten Empty = []
flatten (Leaf l) = [l]
flatten (Node left root right) = flatten left ++ [root] ++ flatten right

balance :: [a] -> Tree a
balance [] = Empty
balance [l] = Leaf l
balance xs  = Node (balance ys) x (balance zs)
  	where
    	n      = length xs `div` 2
    	ys     = take n xs
    	(x:zs) = drop n xs



data Expr = Val Int | Add Expr Expr | Subs Expr Expr
  	deriving (Show, Eq)

folde :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
folde f g h x = case x of (Val v) -> f v
                          (Add a1 a2) -> g (folde f g h a1) (folde f g h a2)
                          (Subs s1 s2) -> h (folde f g h s1) (folde f g h s2)

evalExpr :: Expr -> Int
evalExpr = folde id (+) (-)

size :: Expr -> Int
size (Val _) = 1
size (Add (Val _) (Val _)) = 2
size (Add e1 e2) = size e1 + size e2
size (Subs (Val _) (Val _)) = 2
size (Subs e1 e2) = size e1 + size e2


data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | Equiv Prop Prop
  deriving (Show)

type Assoc k v = [(k,v)]

find :: (Eq k) => k -> Assoc k v -> v
find k [] = error "Key not found!"
find k ((k',x):xs)
  | k == k'   = x
  | otherwise = find k xs

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Equiv p q) = eval s p == eval s q


vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0       = [[]]
bools n | n>0 = map (False:) bss ++ map (True:) bss
  where bss = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = nub (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

isSat :: Prop -> Maybe Subst
isSat p = if null sats
          then Nothing
          else Just $ head sats
          where sats = [s | s <- substs p, eval s p]

abba :: Monoid a => a -> a -> a
abba a b = a <> b <> b <> a


data Option a = None | Some a
  	deriving (Show)

data List a = Nil | Cons a (List a)
  	deriving (Show)

instance Eq a => Eq (Option a) where
	(==) None None         = True
	(==) None (Some _)     = False
	(==) (Some _) None     = False
	(==) (Some x) (Some y) = x== y

instance Eq a => Eq (List a) where
	(==) Nil Nil = True
	(==) Nil (Cons _ _) = False
	(==) (Cons _ _) Nil = False
	(==) (Cons c1 c2) (Cons d1 d2) = c1 == d1 && c2 == d2

--instance Eq a => Eq (Tree a) where
--	(==) (Leaf l) (Leaf l1) = l == l1
--	(==) (Leaf _) (Node _ _ _) = False
--	(==) (Node _ _ _) (Leaf _) = False
--	(==) (Node left root right) (Node left1 root1 right1) = root == root1 && left == left1 && right == right1



data Nat = Zero | Suc Nat
	deriving (Show, Eq)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Suc n) = 1 + natToInteger n

add :: Nat -> Nat -> Nat
add Zero Zero = 0
add (Suc n) (Suc m) = Suc $ Suc $ add n m
add (Suc n) Zero = Suc $ add n Zero
add Zero (Suc m) = Suc $ add Zero m

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult (Suc Zero) x = x
mult x (Suc Zero) = x
mult (Suc n) x = add x (mult n x)

pow :: Nat -> Nat -> Nat
pow Zero _ = Zero
pow x Zero = Suc Zero
pow x (Suc n) = mult x (pow x n)

instance Ord Nat where
  -- (<=) :: Nat -> Nat -> Bool
  	(<=) Zero Zero = True
  	(<=) Zero (Suc _) = True
  	(<=) (Suc _) Zero = False
  	(<=) (Suc x)(Suc y) = x <= y

instance Num Nat where
  -- (+) :: Nat -> Nat -> Nat
  	x + y = add x y

  -- (*) :: Nat -> Nat -> Nat
  	x * y = mult x y

  --- fromInteger :: Integer -> Nat
  	fromInteger 0 = Zero
  	fromInteger n = Suc $ fromInteger (n - 1)

data Square = Square { squareSide :: Double }
  	deriving (Show, Eq)

data Rectangle = Rect { rectWidth :: Double , rectHeight :: Double }
  deriving (Show, Eq)

data Circle = Circle { circleRadius :: Double }
  deriving (Show, Eq)

data Triangle = Triangle { triangleSide1 :: Double, triangleSide2 :: Double, triangleSide3 :: Double }
  deriving (Show, Eq)

data RegularPolygon = Poly { polySides :: Int , polySideLength :: Double }
  deriving (Show, Eq)


class Shape a where
	corners       :: a -> Int
	circumference :: a -> Double
--	surface       :: a -> Double
--	rescale       :: Double -> a -> a

instance Shape Square where
	corners (Square _) = 4
	circumference (Square x) = 4 * x

instance Shape Rectangle where
	corners (Rect _ _) = 4
	circumference (Rect x y) = 2 * (x + y)

instance Shape Circle where
	corners (Circle _) = 0
	circumference (Circle r) = 44 * r / 7

instance Shape Triangle where
	corners (Triangle _ _ _) = 3
	circumference (Triangle a b c) = a + b + c

instance Shape RegularPolygon where
	corners (Poly n _) = n
	circumference (Poly n l) = (fromIntegral n) * l




data Quaternion = Quat Double Double Double Double
	deriving Eq

-- Take the real part of a quaternion (used for testing)
-- realPart (a + b*i + c*j + d*k) == a
realPart :: Quaternion -> Double
realPart (Quat a _ _ _) = a

i, j, k :: Quaternion
i = Quat 0 1 0 0
j = Quat 0 0 1 0
k = Quat 0 0 0 1

fromDouble :: Double -> Quaternion
fromDouble x = Quat x 0 0 0

instance Show Quaternion where
  	show (Quat a b c d) = show a ++ " + " ++ show b ++ "i + " ++ show c ++ "j + " ++ show d ++ "k"

instance Num Quaternion where
  	(Quat a b c d) + (Quat a1 b1 c1 d1) = Quat (a + a1) (b + b1) (c + c1) (d + d1)
  	(Quat a b c d) * (Quat e f g h) = Quat (a*e - b*f - c*g - d*h) (a*f + b*e + c*h - d*g) (a*g - b*h + c*e + d*f) (a*h + b*g - c*f + d*e)
  	abs (Quat a b c d) = Quat (sqrt (a*a + b*b + c*c + d*d)) 0 0 0
  	signum (Quat a b c d) = Quat (a/e) (b/e) (c/e) (d/e)
                            where e = sqrt (a^2+b^2+c^2+d^2)
  	negate (Quat a b c d) = Quat (-a) (-b) (-c) (-d)
  	fromInteger n = Quat (fromInteger n) 0 0 0



data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Show)
              
getInt :: Integral a => JValue -> Maybe a
getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _         = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject o) = Just o
getObject _           = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray a) = Just a
getArray _          = Nothing

isNull :: JValue -> Bool
isNull v            = v == JNull



renderJValue :: JValue -> String

renderJValue (JString s)   = show s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue JNull         = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
  where pairs [] = ""
        pairs ps = intercalate ", " (map renderPair ps)
        renderPair (k,v)   = show k ++ ": " ++ renderJValue v

renderJValue (JArray a) = "[" ++ values a ++ "]"
  where values [] = ""
        values vs = intercalate ", " (map renderJValue vs)