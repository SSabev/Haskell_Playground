-- INF 1 Functional Programming
-- 
-- Indexed data represented as a tree


module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList
                  )

where

-- Modules for testing

import Test.QuickCheck
import Data.List

-- The data type

data Ord k => Keymap k a = Node k a (Keymap k a) (Keymap k a)
                         | Leaf

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

-- Exercise 5

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) = 1 + max (depth left) (depth right) 

-- Exercise 6

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf  = []
toList (Node a b left right) = toList left ++ (a,b):(toList right)

-- Exercise 7

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = f 
    where
      f Leaf = Node key value Leaf Leaf
      f (Node k v left right) | key == k  = Node k value left right
                              | key <= k  = Node k v (f left) right
                              | otherwise = Node k v left (f right)

-- exercise 8

get :: Ord k => k -> Keymap k a -> Maybe a
get key = f
    where
      f Leaf = undefined
      f (Node k v left right) | key == k  = undefined
                              | key <= k  = undefined
                              | otherwise = undefined

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- exercise 9

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList = undefined


prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where
      zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where
      zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 11

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge Leaf t = t
merge t Leaf = t
merge x1@(Node k1 v1 l1 r1) x2@(Node k2 v2 l2 r2)
    | k1 == k2  = Node k1 v1 (merge l1 l2) (merge r1 r2)
    | k1 <= k2  = Node k1 v1 l1 (merge x2 r1)
    | otherwise = Node k1 v1 (merge x2 l1) r1

-- Exercise 12

del :: Ord k => k -> Keymap k a -> Keymap k a
del = undefined

-- Exercise 13

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select = undefined 

