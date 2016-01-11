{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Renderable (
    Primitive(..),
    Element(..),
    Composite(..),
    Renderer,
    Rendering,
    CleanOp,
    Cache,
    renderData,
    emptyRenderer,
    appendRenderer
) where

import Prelude hiding (lookup)
import Control.Arrow (first)
import Control.Monad
import Data.Hashable
import Data.IntMap (IntMap)
import qualified Data.Foldable as F
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
-- Element
--------------------------------------------------------------------------------
-- | Element is an existential type that can be used to enclose
-- instances of Primitive in order to contain them all in a heterogeneous list.
-- 'm', 'r' and 't' must be shared with all Primitive instances stored in
-- the heterogeneous list of Elements.
data Element m r t where
    Element  :: ( Monad m, Hashable a, Primitive a
                , m ~ PrimM a
                , r ~ PrimR a
                , t ~ PrimT a)
             => a -> Element m r t

instance Hashable (Element m r t) where
    hashWithSalt s (Element a) = s `hashWithSalt` "Element" `hashWithSalt` a

instance Eq (Element m r t) where
    a == b = hash a == hash b
--------------------------------------------------------------------------------
-- Compositing
--------------------------------------------------------------------------------
-- | A 'Composite' is a type that can be broken down into a collection of
-- transformed primitives.
class Composite a f m r t where
    -- | Break down a 'Composite' into a heterogeneous list of transformed
    -- primitives.
    composite :: a -> f (t, Element m r t)
--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------
-- | A Rendering is an effectful computation for displaying something given a
-- transform.
type Rendering m t = t -> m ()

-- A CleanOp  an effectfull computaton that cleans up any resources allocated
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

findRenderer :: Monad m
             => Cache m t
             -> (Cache m t, IntMap (Element m r t))
             -> Element m r t
             -> (Cache m t, IntMap (Element m r t))
findRenderer cache (found, missing) (Element a) =
    let k = hash a in
    case IM.lookup k cache of
        Nothing -> (found, IM.insert k (Element a) missing)
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

getElementRenderer :: r -> Cache m t -> Element m r t -> m (Cache m t)
getElementRenderer rez cache (Element a) = getRenderer rez cache a

clean :: Renderer m t -> m ()
clean = fst

render :: Renderer m t -> t -> m ()
render = snd

renderElement :: Monad m => Cache m t -> t -> Element m r t -> m ()
renderElement cache t (Element a) = do
    let k = hash a
    case IM.lookup k cache of
        Nothing -> return ()
        Just r  -> render r t

-- | Render a datatype using renderings stored in the given cache, return a
-- new cache that can be used to render the next datatype.
renderData :: forall proxy f a m r t.
           (Traversable f, Composite a f m r t, Monad m, Monoid t)
           => r -> Cache m t -> a -> proxy f -> m (Cache m t)
renderData rez cache a _ = do
        -- comp is a heterogeneous list of all the primitives needed to render
        -- this datatype  'a'.
    let func = composite :: a -> f (t, Element m r t)
        comp = func a
        (found, missing) = foldl (findRenderer cache)
                                 (mempty, mempty)
                                 (map snd $ F.toList comp)
        stale = cache `IM.difference` found

    -- Clean the stale renderers
    sequence_ $ fmap clean stale

    -- Get the missing renderers
    new <- foldM (getElementRenderer rez) mempty $ IM.elems missing

    let next = IM.union found new

    --liftIO $ do putStrLn $ "Prev:    " ++ show (IM.keys cache)
    --            putStrLn $ "Found:   " ++ show (IM.keys found)
    --            putStrLn $ "Missing: " ++ show (IM.keys missing)
    --            putStrLn $ "Stale:   " ++ show (IM.keys stale)
    --            putStrLn $ "Next:    " ++ show (IM.keys next)

    -- Render the composite
    mapM_ (uncurry $ renderElement next) comp
    return next
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
-- | Any Element is a composite of itself if its transform type is a monoid.
instance (Monoid t, Applicative f) => Composite (Element m r t) f m r t where
    composite e = pure (mempty, e)

-- | A tuple is a composite if its right type is a composite and the
-- left type is the transform and that transform is a monoid. In this case the
-- result is the right type transformed by the left type.
instance (Monoid t, Functor f, Composite a f m r t)
    => Composite (t,a) f m r t where
    composite (t, a) = fmap (first (mappend t)) (composite a)

-- | A Maybe is a composite if its contained type is a composite and if the
-- iteration container is a list. The result is is the composite of its
-- contained type or an empty list.
instance Composite a [] m r t => Composite (Maybe a) [] m r t where
    composite (Just a) = composite a
    composite _ = []

-- | A list is a composite by compositing each element and concatenating
-- the result.
instance Composite a [] m r t => Composite [a] [] m r t where
    composite = concatMap composite
