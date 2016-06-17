
module Data.Cache(
    createCache,
    cache,
    fetch,
    fetchCached,
    filterCached,
    count
) where

import Data.Cache.Types

import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar
import Control.Lens (at, review, set, view, (^.))
import Control.Monad.Except (MonadError)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Foldable (minimumBy)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes)
import Data.Time.Clock.POSIX (getPOSIXTime)

-- |Create a cache
createCache :: (MonadIO m, Eq k, Ord k, Hashable k) => Int -> Int -> (k -> m v) -> STM (Cache m k v)
createCache maxItems maxAge lu = do
    items <- newTVar HM.empty
    return $ Cache {
        _cacheLookup = lu,
        _cacheItems = items,
        _cacheMaxItems = maxItems,
        _cacheMaxAge = maxAge
    }

-- |Cache an item
cache :: (MonadIO m, Eq k, Ord k, Hashable k) => Cache m k v -> k -> v -> m ()
cache c k v = liftIO $ do
    t <- time
    atomically . modifyTVar (c ^. cacheItems) $ HM.insert k (Item k t v)

-- |Fetch an item
fetch :: (MonadIO m, Eq k, Ord k, Hashable k) => k -> Cache m k v -> m v
fetch k c = maybe (fetchM k c) return =<< fetchCached k c

-- |Fetch an item if it exists in the cache
fetchCached :: (MonadIO m, Eq k, Ord k, Hashable k) => k -> Cache m k v -> m (Maybe v)
fetchCached k c = liftIO $ do
    let maxAge = c ^. cacheMaxAge
    t <- time
    mi <- atomically $ HM.lookup k <$> readTVar (c ^. cacheItems)
    return $ do
        (Item _ created val) <- mi
        case (created + maxAge > t) of
            True -> Just val
            False -> Nothing

fetchM :: (MonadIO m, Eq k, Ord k, Hashable k) => k -> Cache m k v -> m v
fetchM k c = do
    v <- (c ^. cacheLookup) k
    t <- time
    liftIO . atomically . modifyTVar (c ^. cacheItems) $ purge c t . set (at k) (Just $ Item k t v)
    return v

purge :: (Eq k, Hashable k) => Cache m k v -> Int -> HM.HashMap k (Item k v) -> HM.HashMap k (Item k v)
purge c tme = trim maxItems . HM.filter (checkAge maxAge tme)
    where
    maxItems = c ^. cacheMaxItems
    maxAge = c ^. cacheMaxAge

checkAge :: Int -> Int -> Item k v -> Bool
checkAge maxAge tme (Item _ created v) = created + maxAge > tme

trim :: (Eq k, Hashable k) => Int -> HM.HashMap k (Item k v) -> HM.HashMap k (Item k v)
trim maxSize m = case (HM.size m > 0 && HM.size m > maxSize) of
    False -> m
    True -> trim maxSize $ HM.delete k m where k = (view itemK $ minimumBy minCreated m)

minCreated :: (Eq k) => Item k v -> Item k v -> Ordering
minCreated i i' = compare i i'

time :: MonadIO m => m Int
time = liftIO $ round <$> getPOSIXTime

-- |Find a cache item
filterCached :: MonadIO m => (v -> Bool) -> Cache m k v -> m [v]
filterCached f c = liftIO $ do
    let maxAge = c ^. cacheMaxAge
    t <- time
    items <- atomically $ readTVar (c ^. cacheItems)
    let ix = snd <$> HM.toList (HM.filter (f . view itemValue) items)
    return $ view itemValue <$> filter (checkAge maxAge t) ix

-- |Get cache size
count :: MonadIO m => Cache m k v -> m Int
count c = liftIO . atomically $ do
    items <- readTVar (c ^. cacheItems)
    return $ HM.size items
