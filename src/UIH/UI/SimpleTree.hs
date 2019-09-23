{-# LANGUAGE StandaloneDeriving #-}
module UIH.UI.SimpleTree where

import Data.Foldable
import Data.Traversable

-- simple tree to handle UIs
-- [] is a terminating element
data MultiTree a = MultiTreeNode a [MultiTree a] | EmptyTree

instance Foldable MultiTree where 
    -- foldMap :: Monoid m => (a -> m) -> SimpleTree a -> m
    foldMap f EmptyTree = mempty
    foldMap f (MultiTreeNode x []) = f x
    foldMap f (MultiTreeNode x  children) = f x <> foldMap (foldMap f) children -- took an hour to figure out!!! 

instance Functor MultiTree where
    -- fmap :: (a -> b) -> MultiTree a -> MultiTree b
    fmap f EmptyTree = EmptyTree
    fmap f (MultiTreeNode x []) = MultiTreeNode (f x) []
    fmap f (MultiTreeNode x children) = MultiTreeNode (f x) (fmap (fmap f) children)

instance Traversable MultiTree where
    -- traverse :: Applicative f => (a -> f b) -> MultiTree a -> f (MultiTree b)
    -- (<*>) :: f (a -> b) -> f a -> f b 
    -- sequenceA :: Applicative f => MultiTree (f a) -> f (MultiTree a)
    traverse g EmptyTree = pure EmptyTree
    traverse g (MultiTreeNode x []) = MultiTreeNode <$> g x <*> (pure [])
    traverse g (MultiTreeNode x children) = MultiTreeNode <$> g x <*> (traverse (traverse g) children)

ttt = MultiTreeNode 1 [MultiTreeNode 2 [], MultiTreeNode 3 [], MultiTreeNode 4 [MultiTreeNode 5 []]]

instance Semigroup Integer where (<>) = (+)
instance Monoid Integer where mempty = 0

instance Show a => Show (MultiTree a) where
    show x = foldMap (\el -> show el ++ ", ") x
    