module Data.BKTree
  ( BKTree
  , MetricSpace(..)
  , Distance

  , empty
  , null
  , size
  , singleton
  , insert
  , member
  , notMember
  , query
  , lookup

  , fromList
  , Data.BKTree.toList
  ) where

import Data.Monoid
import Data.Foldable hiding (foldr,sum,concatMap)

import Data.Maybe (isJust,listToMaybe)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Prelude hiding (lookup,null)

type Distance = Int -- questionable

class MetricSpace a where
  distance :: a -> a -> Distance

-- Dummy instance for testing.
instance MetricSpace Int where
  distance x y = abs (y - x)

data BKTree a = Node a !(IntMap (BKTree a))
              | Empty
  deriving Show

instance Foldable BKTree where
  foldMap _ Empty          = mempty
  foldMap f (Node x edges) = mconcat (f x : map (foldMap f) childs)
    where childs = IM.elems edges

empty :: MetricSpace a => BKTree a
empty = Empty

null :: MetricSpace a => BKTree a -> Bool
null Empty = True
null _     = False

size :: MetricSpace a => BKTree a -> Int
size Empty          = 0
size (Node _ edges) = 1 + sum (map size . IM.elems $ edges)

singleton :: MetricSpace a => a -> BKTree a
singleton x = Node x IM.empty

insert :: MetricSpace a => a -> BKTree a -> BKTree a
insert x Empty           = singleton x
insert x (Node x0 edges) = Node x0 (update edges)
  where update = IM.insertWith (const (insert x)) d (singleton x)
        d      = distance x x0

member :: MetricSpace a => a -> BKTree a -> Bool
member x = isJust . lookup x

notMember :: MetricSpace a => a -> BKTree a -> Bool
notMember x = not . member x

query :: MetricSpace a => a -> Distance -> BKTree a -> [a]
query _ _ Empty           = []
query x n (Node x0 edges) = if d <= n then x0 : go else go
  where go  = concatMap (query x n) (IM.elems . cut (d-n,d+n) $ edges)
        d   = distance x x0
        cut (x,y) = fst . IM.split (y+1) . snd . IM.split (x-1)

lookup :: MetricSpace a => a -> BKTree a -> Maybe a 
lookup x = listToMaybe . query x 0

fromList :: MetricSpace a => [a] -> BKTree a
fromList = foldr insert empty

toList :: MetricSpace a => BKTree a -> [a]
toList = Data.Foldable.toList
