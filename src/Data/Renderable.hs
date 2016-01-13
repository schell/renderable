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
    renderPrimitives,
    emptyRenderer,
    appendRenderer
) where

import Prelude hiding (lookup)
import Control.Monad
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

-- | Render a list of primitives using renderings stored in the given cache,
-- return a new cache that can be used to render the next list of
-- primitives.
renderPrimitives :: (Primitive a, Monad (PrimM a), Monoid (PrimT a), Hashable a)
                 => PrimR a -> Cache (PrimM a) (PrimT a) -> [(PrimT a, a)]
                 -> (PrimM a) (Cache (PrimM a) (PrimT a))
renderPrimitives rez cache prims = do
    let (found, missing) = foldl (findRenderer cache)
                                 (mempty, mempty)
                                 (map snd prims)
        stale = cache `IM.difference` found

    -- Clean the stale renderers
    sequence_ $ fmap clean stale

    -- Get the missing renderers
    new <- foldM (getRenderer rez) mempty $ IM.elems missing

    let next = IM.union found new

    --liftIO $ do putStrLn $ "Prev:    " ++ show (IM.keys cache)
    --            putStrLn $ "Found:   " ++ show (IM.keys found)
    --            putStrLn $ "Missing: " ++ show (IM.keys missing)
    --            putStrLn $ "Stale:   " ++ show (IM.keys stale)
    --            putStrLn $ "Next:    " ++ show (IM.keys next)

    -- Render the composite
    mapM_ (uncurry $ renderElement next) prims
    return next
