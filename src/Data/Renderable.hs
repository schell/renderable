{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Renderable (
    Primitive(..),
    Renderer,
    Rendering,
    CleanOp,
    Cache,
    CacheStats(..),
    renderPrims,
    renderPrimsDebug,
    renderPrimsWithStats,
    emptyRenderer,
    appendRenderer
) where

import Prelude hiding (lookup)
import Control.Monad
import Control.Monad.IO.Class
import Data.Hashable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
--------------------------------------------------------------------------------
-- Primitives
--------------------------------------------------------------------------------
-- | A 'Primitive' is the smallest thing can can be rendered in your graphics
-- system. Some examples are points, lines, triangles and other shapes.
class Primitive a where
    -- | The monad in which rendering calls will take place.
    type PrimM a :: * -> *
    -- | The type of the graphics transformation.
    type PrimT a :: *
    -- | The datatype that holds cached resources such as references to
    -- windows, shaders, etc.
    type PrimR a :: *
    -- | Return whether resources can currently be allocated for the primitive.
    -- Return False to defer compilation until a later time (the next
    -- frame).
    canAllocPrimitive :: PrimR a -> a -> Bool
    canAllocPrimitive _ _ = True
    -- | Allocate resources for rendering the primitive and return
    -- a monadic call that renders the primitive using a transform. Tuple
    -- that with a call to clean up the allocated resources.
    compilePrimitive :: Monad (PrimM a)
                     => PrimR a
                     -> a
                     -> (PrimM a) (Renderer (PrimM a) (PrimT a))
--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------
-- | A Rendering is an effectful computation for displaying something given a
-- transform.
type Rendering m t = t -> m ()

-- A CleanOp is an effectfull computaton that cleans up any resources allocated
-- during the creation of an associated Rendering.
type CleanOp m = m ()

-- A Renderer is the pairing of a Rendering and a Cleanup.
type Renderer m t = (CleanOp m, Rendering m t)

emptyRenderer :: Monad m => Renderer m t
emptyRenderer = (return (), const $ return ())

appendRenderer :: Monad m => Renderer m t -> Renderer m t -> Renderer m t
appendRenderer (c1,r1) (c2,r2) = (c1 >> c2, \t -> r1 t >> r2 t)

-- | A cache of renderers.
type Cache m t = IntMap (Renderer m t)

findRenderer :: (Monad m, Hashable a)
             => Cache m t
             -> (Cache m t, IntMap a)
             -> a
             -> (Cache m t, IntMap a)
findRenderer cache (found, missing) a =
    let k = hash a in
    case IM.lookup k cache of
        Nothing -> (found, IM.insert k a missing)
        Just r  -> (IM.insert k r found, missing)

getRenderer :: (Primitive a, Hashable a, Monad (PrimM a))
            => PrimR a
            -> Cache (PrimM a) (PrimT a)
            -> a
            -> (PrimM a) (Cache (PrimM a) (PrimT a))
getRenderer rez cache a =
    if canAllocPrimitive rez a
    then do r <- compilePrimitive rez a
            return $ IM.insert (hash a) r cache
    else return cache

clean :: Renderer m t -> m ()
clean = fst

render :: Renderer m t -> t -> m ()
render = snd

renderElement :: (Hashable a, Monad m) => Cache m t -> t -> a -> m ()
renderElement cache t a = do
    let k = hash a
    case IM.lookup k cache of
        Nothing -> return ()
        Just r  -> render r t

-- | A sum of lists of rendering hashes between two cache states. 
-- Used for debugging resource management.
data CacheStats a = CacheStats { cachedPrev    :: [Int]
                               -- ^ All the keys of the previous cache state. 
                               , cachedFound   :: [Int]
                               -- ^ The keys needed for the next state that
                               -- were found in the previous cache (no need
                               -- to allocate).
                               , cachedMissing :: [Int]
                               -- ^ The keys needed for the next state that
                               -- were not found in the previous cache (these
                               -- will need allocating).
                               , cachedStale   :: [Int]
                               -- ^ The keys found in the previous cache that
                               -- are not needed for the next state (these
                               -- can be deallocated).
                               , cachedNext    :: [Int] 
                               -- ^ All the keys of the next cache state.
                               }

-- | Map a 'CacheStats' into a nice readable string.
showCacheStats :: CacheStats a -> String
showCacheStats (CacheStats cache found missing stale next) = unlines
    [ "Prev:    " ++ show cache
    , "Found:   " ++ show found 
    , "Missing: " ++ show missing
    , "Stale:   " ++ show stale
    , "Next:    " ++ show next
    ]

-- | Render a list of primitives using renderings stored in the given cache,
-- return a new cache that can be used to render the next list of
-- primitives, along with some info about the comparison of the given and
-- returned cache. 
renderPrimsWithStats :: (Primitive a, Monad (PrimM a), Monoid (PrimT a), Hashable a) 
                     => PrimR a -> Cache (PrimM a) (PrimT a) -> [(PrimT a, a)] 
                     -> (PrimM a) (Cache (PrimM a) (PrimT a), CacheStats a)
renderPrimsWithStats rez cache prims = do
    let (found, missing) = foldl (findRenderer cache)
                                 (mempty, mempty)
                                 (map snd prims)
        stale = cache `IM.difference` found

    -- Clean the stale renderers
    sequence_ $ fmap clean stale

    -- Get the missing renderers
    new <- foldM (getRenderer rez) mempty $ IM.elems missing

    let next = IM.union found new
        stats = CacheStats { cachedPrev = IM.keys cache
                           , cachedFound = IM.keys found
                           , cachedMissing = IM.keys missing
                           , cachedStale = IM.keys stale
                           , cachedNext = IM.keys next
                           }

    -- Render the composite
    mapM_ (uncurry $ renderElement next) prims
    return (next,stats)

-- | Render a list of primitives using renderings stored in the given cache,
-- return a new cache that can be used to render the next list of
-- primitives. Optionally print some debug info.
renderPrimsDebug :: (Primitive a, MonadIO (PrimM a), Monoid (PrimT a), Hashable a) 
                 => Bool 
                 -> PrimR a -> Cache (PrimM a) (PrimT a) -> [(PrimT a, a)] 
                 -> (PrimM a) (Cache (PrimM a) (PrimT a))
renderPrimsDebug debug rez cache prims = do
    (next, stats) <- renderPrimsWithStats rez cache prims
    when debug $ liftIO $ putStrLn $ showCacheStats stats
    return next

-- | Render a list of primitives using renderings stored in the given cache,
-- return a new cache that can be used to render the next list of
-- primitives.
renderPrims :: (Primitive a, Monad (PrimM a), Monoid (PrimT a), Hashable a)
            => PrimR a -> Cache (PrimM a) (PrimT a) -> [(PrimT a, a)]
            -> (PrimM a) (Cache (PrimM a) (PrimT a))
renderPrims rez cache prims = fst <$> renderPrimsWithStats rez cache prims
