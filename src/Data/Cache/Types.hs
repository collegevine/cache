{-# LANGUAGE TemplateHaskell #-}

module Data.Cache.Types where

import Control.Concurrent.STM.TVar (TVar)
import Control.Lens.TH
import Control.Lens.Type (Prism')
import Data.HashMap.Strict (HashMap)

data Cache m k v = Cache {
    _cacheLookup :: k -> m v,
    _cacheItems :: TVar (HashMap k (Item k v)),
    _cacheMaxItems :: Int,
    _cacheMaxAge :: Int
}

data Item k v = Item {
    _itemK :: k,
    _itemCreated :: Int,
    _itemValue :: v
}

instance Eq k => Eq (Item k v) where
    (==) (Item k _ _) (Item k' _ _) = k == k'

instance Eq k => Ord (Item k v) where
    compare (Item _ c _) (Item _ c' _) = compare c c'

makeLenses ''Cache
makeLenses ''Item
