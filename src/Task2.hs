{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment

import Task1 (Tree (..))
import Prelude hiding (Ordering (..), compare, foldl, foldr)

-- * Type definitions

-- | Ordering enumeration
data Ordering = LT | EQ | GT
  deriving (Show)

-- | Binary comparison function indicating whether first argument is less, equal or
-- greater than the second one (returning 'LT', 'EQ' or 'GT' respectively)
type Cmp a = a -> a -> Ordering

-- * Function definitions

-- | Binary comparison function induced from `Ord` constraint
--
-- Usage example:
--
-- >>> compare 2 3
-- LT
-- >>> compare 'a' 'a'
-- EQ
-- >>> compare "Haskell" "C++"
-- GT
compare :: (Ord a) => Cmp a
compare a b
  | a < b = LT
  | a > b = GT
  | otherwise = EQ

isLT :: Cmp a -> a -> a -> Bool
isLT cmp a b = case cmp a b of
  LT -> True
  _ -> False

isGT :: Cmp a -> a -> a -> Bool
isGT cmp a b = case cmp a b of
  GT -> True
  _ -> False

-- | Conversion of list to binary search tree
-- using given comparison function
--
-- Usage example:
--
-- >>> listToBST compare [2,3,1]
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> listToBST compare ""
-- Leaf
listToBST :: Cmp a -> [a] -> Tree a
listToBST cmp (x:xs) = tinsert cmp x (listToBST cmp xs)
listToBST _ [] = Leaf

-- | Conversion from binary search tree to list
--
-- Resulting list will be sorted
-- if given tree is valid BST with respect
-- to some 'Cmp' comparison.
--
-- Usage example:
--
-- >>> bstToList (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- [1,2,3]
-- >>> bstToList Leaf
-- []
bstToList :: Tree a -> [a]
bstToList (Branch k l r) = bstToList l ++ [k] ++ bstToList r
bstToList Leaf = []

-- | Tests whether given tree is a valid binary search tree
-- with respect to given comparison function
--
-- Usage example:
--
-- >>> isBST compare (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- True
-- >>> isBST compare (Leaf :: Tree Char)
-- True
-- >>> isBST compare (Branch 5 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- False
isBST :: Cmp a -> Tree a -> Bool
isBST cmp tree = isBSTHelper cmp tree Nothing Nothing
  where
    isBSTHelper :: Cmp a -> Tree a -> Maybe a -> Maybe a -> Bool
    isBSTHelper _ Leaf _ _ = True
    isBSTHelper cmp' (Branch k l r) minVal maxVal = withinBounds && isBSTHelper cmp' l minVal (Just k) && isBSTHelper cmp' r (Just k) maxVal
      where
        withinBounds = maybe True (isGT cmp' k) minVal && maybe True (isLT cmp' k) maxVal

-- | Searches given binary search tree for
-- given value with respect to given comparison
--
-- Returns found value (might not be the one that was given)
-- wrapped into 'Just' if it was found and 'Nothing' otherwise.
--
-- Usage example:
--
-- >>> tlookup compare 2 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Just 2
-- >>> tlookup compare 'a' Leaf
-- Nothing
-- >>> tlookup (\x y -> compare (x `mod` 3) (y `mod` 3)) 5 (Branch 2 (Branch 0 Leaf Leaf) (Branch 2 Leaf Leaf))
-- Just 2
tlookup :: Cmp a -> a -> Tree a -> Maybe a
tlookup _ _ Leaf = Nothing
tlookup cmp value (Branch k l r) =
  case cmp value k of
    EQ -> Just k
    LT -> tlookup cmp value l
    GT -> tlookup cmp value r

-- | Inserts given value into given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- If the same value with respect to comparison
-- was already present in the 'Tree' then replaces it with given value.
--
-- Usage example:
--
-- >>> tinsert compare 0 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 (Branch 0 Leaf Leaf) Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 'a' Leaf
-- Branch 'a' Leaf Leaf
tinsert :: Cmp a -> a -> Tree a -> Tree a
tinsert _ value Leaf = Branch value Leaf Leaf
tinsert cmp value (Branch k l r) =
  case cmp value k of
    EQ -> Branch value l r
    LT -> Branch k (tinsert cmp value l) r
    GT -> Branch k l (tinsert cmp value r)

-- | Deletes given value from given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- Returns updated 'Tree' if the value was present in it;
-- or unchanged 'Tree' otherwise.
--
-- Usage example:
--
-- >>> tdelete compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 Leaf (Branch 3 Leaf Leaf)
-- >>> tdelete compare 'a' Leaf
-- Leaf
tdelete :: Cmp a -> a -> Tree a -> Tree a
tdelete _ _ Leaf = Leaf
tdelete cmp value (Branch k l r) =
  case cmp value k of
    EQ -> deleteNode cmp (Branch k l r)
    LT -> Branch k (tdelete cmp value l) r
    GT -> Branch k l (tdelete cmp value r)

findMin :: Tree a -> a
findMin Leaf = error "Cannot find min in empty tree"
findMin (Branch k Leaf _) = k
findMin (Branch _ l _) = findMin l

deleteNode :: Cmp a -> Tree a -> Tree a
deleteNode _ Leaf = Leaf
deleteNode _ (Branch _ l Leaf) = l
deleteNode _ (Branch _ Leaf r) = r
deleteNode cmp (Branch _ l r) = Branch (findMin r) l (tdelete cmp (findMin r) r)