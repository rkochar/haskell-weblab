module Reader where

newtype Reader r a = Reader (r -> a)

-- The ask function gets the value of the global variable stored
-- in the Reader.
ask :: Reader r r
ask = Reader id

-- The asks function gets the value of the global variable and
-- applies the given function to it.
asks :: (r -> a) -> Reader r a
asks = Reader

-- The local function allows running a Reader action with a
-- different value of the local variable.
local :: (r -> r) -> Reader r a -> Reader r a
local g (Reader f) = Reader (f . g)

-- The runReader function unwraps a Reader r a value and returns
-- it as a function from r to a.
runReader :: Reader r a -> r -> a
runReader (Reader f) = f

instance Functor (Reader r) where
  -- fmap :: (a -> b) -> Reader r a -> Reader r b
  	fmap f (Reader g) = Reader $ f . g

instance Applicative (Reader r) where
  -- pure :: a -> Reader r a
  	pure x = Reader $ const x

  -- (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  	(Reader f) <*> (Reader g) = Reader $ f <*> g

instance Monad (Reader r) where
  -- return :: a -> Reader r a
  	return = pure

  -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  	Reader f >>= g = Reader $ \x -> runReader (g $ f x) x



add2AndShowDouble :: Reader Int String
add2AndShowDouble = do
    x <- ask
    return $ show (x + 2, x * 2)



data User = User
  { userEmail :: String
  , userPassword :: String
  , userName :: String
  , userAge :: Int
  , userBio :: String
  }

checkPassword :: String -> Reader User Bool
checkPassword s = do
    pwd <- ask
    return (userPassword pwd == s)

displayProfile :: Reader User [String]
displayProfile = do
    name <- asks userName
    age <- asks userAge
    bio <- asks userBio
    return ["Name: " ++ name, "Age: " ++ show age, "Bio: " ++ bio]

authAndDisplayProfile :: User -> String -> Maybe [String]
authAndDisplayProfile user string =
    if runReader (checkPassword string) user
    then Just $ runReader displayProfile user
    else Nothing