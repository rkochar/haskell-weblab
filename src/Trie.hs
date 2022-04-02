module Trie where

import Data.List
  
data Trie = Node Bool [(Char,Trie)]
	deriving (Show)

example = Node False [('f', Node False [('o', Node False [('o', Node True [])])]),
                      ('b', Node False [('a', Node False [('r', Node True []),
		              ('z', Node True [])])])]

-- {"","a","ab","ba"}
myTrie :: Trie
myTrie = Node True [('a', Node True [('b', Node True [])]),
                    ('b', Node False [('a', Node True [])])]

singletonTrie :: String -> Trie
singletonTrie "" = Node True []
singletonTrie [s] = Node True [(s, Node True [])]
singletonTrie (s : ss) = Node False [(s, singletonTrie ss)]

elemTrie :: String -> Trie -> Bool
elemTrie "" (Node True _) = True
elemTrie "" _ = False
elemTrie _ (Node True []) = False
elemTrie [s] (Node True xs) = case lookup s xs of Nothing -> False
                                                  Just x  -> elemTrie "" x
elemTrie (s : ss) (Node _ xs) = case lookup s xs of Nothing -> False
                                                    Just x  -> elemTrie ss x
  				
  				
emptyTrie :: Trie
emptyTrie = Node False []
s = Node True []
a = Node False [('a', s)]
b = Node False [('b', s)]

--merge :: Trie -> Trie -> [(Char, Trie)]
--merge (Node b1 ts1) (Node b2 ts2) = Node (b1 || b2) tries
--                                    where tries =  

mergeTrie :: Trie -> Trie -> Trie
mergeTrie (Node b1 ts1) (Node b2 ts2) = 
  	Node (b1 || b2) ([(x,maybe t1 (mergeTrie t1) (lookup x ts2)) | (x,t1) <- ts1] ++ [(x,t2) | (x,t2) <- ts2, isNothing (lookup x ts1)])
  	where
    	isNothing Nothing = True
    	isNothing _       = False
--mergeTrie (Node b1 []) (Node b2 []) = Node (b1 || b2) []
--mergeTrie (Node b1 x) (Node b2 []) = Node (b1 || b2) x
--mergeTrie (Node b1 []) (Node b2 x) = Node (b1 || b2) x
--mergeTrie (Node b1 ts1) (Node b2 ts2) = Node (b1 || b2) (merge ts1 ts2)


instance Semigroup Trie where
  	(<>) = mergeTrie
  
instance Monoid Trie where
  	mempty = emptyTrie
  	mappend = (<>)
  	
  	

{-
The three laws are the following:

    mempty <> t2 == t2
    t1 <> mempty == t1
    (t1 <> t2) <> t3 == t1 <> (t2 <> t3)

We can see that our implementation of Monoid Trie satisfies the first law as follows:

mempty <> Node b ts
= mergeTrie emptyTrie (Node b ts)
= mergeTrie (Node False []) (Node b ts)
= Node (False || b) ([ (x,maybe t1 (mergeTrie t1) (lookup x ts)) | (x,t1) <- []  ]
                  ++ [ (x,t2) | (x,t2) <- ts, isNothing (lookup x []) ])
= Node b ([ (x,maybe t1 (mergeTrie t1) (lookup x ts)) | (x,t1) <- []  ]
       ++ [ (x,t2) | (x,t2) <- ts, isNothing (lookup x []) ])
= Node b ([] ++ [ (x,t2) | (x,t2) <- ts, isNothing (lookup x []) ])
= Node b ([ (x,t2) | (x,t2) <- ts, isNothing (lookup x []) ])
= Node b ([ (x,t2) | (x,t2) <- ts ])
= Node b ts

In words, we can note the following three facts:
1. The boolean component of mergeTrie emptyTrie (Node b ts) is False || b, which is equal to False.
2. The first list in the right-hand side becomes [ (x,maybe t1 (mergeTrie t1) (lookup x ts)) | (x,t1) <- [] ], which is empty.
3. The second list becomes [ (x,t2) | (x,t2) <- ts, isNothing (lookup x []) ], which is just ts since lookup x [] is always Nothing.
-}			    