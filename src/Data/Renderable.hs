{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Renderable (
    Primitive(..),
    Element(..),
    Composite(..),
    Rendering,
    Cache,
    renderData
) where

import Prelude hiding (lookup)
import Control.Arrow (first)
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
    -- | Allocate resources for rendering the primitive and return
    -- a monadic call that renders the primitive using a transform. Tuple
    -- that with a call to clean up the allocated resources.
    compilePrimitive :: Monad (PrimM a)
                     => PrimR a
                     -> a
                     -> (PrimM a) (Rendering (PrimM a) (PrimT a))
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
-- | A 'Composite' is a type that can be broken down into a list of
-- transformed primitives.
class Composite a m r t where
    -- | Break down a 'Composite' into a heterogeneous list of transformed
    -- primitives.
    composite :: a -> [(t, Element m r t)]
--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------
-- | A rendering is a type that contains some effectful computation for
-- displaying something given a transform. It also contains an effectful
-- computation for cleaning up any resources allocated during its creation.
type Rendering m t = (m (), t -> m ())

-- | A cache of renderings.
type Cache m t = IntMap (Rendering m t)

instance Monad m => Monoid (Rendering m t) where
    (ca, fa) `mappend` (cb, fb) = (ca >> cb, \t -> fa t >> fb t)
    mempty = (return (), const $ return ())

findRenderer :: Monad m
             => Cache m t
             -> (Cache m t, IntMap (Element m r t))
             -> Element m r t
             -> (Cache m t, IntMap (Element m r t))
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
getRenderer rez cache a = do
    r <- compilePrimitive rez a
    return $ IM.insert (hash a) r cache

getElementRenderer :: r -> Cache m t -> Element m r t -> m (Cache m t)
getElementRenderer rez cache (Element a) = getRenderer rez cache a

clean :: Rendering m t -> m ()
clean = fst

render :: Rendering m t -> t -> m ()
render = snd

renderElement :: Monad m => Cache m t -> t -> Element m r t -> m ()
renderElement cache t (Element a) = do
    let k = hash a
    case IM.lookup k cache of
        Nothing -> return ()
        Just r  -> render r t

-- | Render a datatype using renderings stored in the given cache, return a
-- new cache that can be used to render the next datatype.
renderData :: (Composite a m r t, Hashable a, Monad m, Monoid t)
           => r -> Cache m t -> a -> m (Cache m t)
renderData rez cache a = do
        -- comp is a heterogeneous list of all the primitives needed to render
        -- this datatype  'a'.
    let comp = composite a
        (found, missing) = foldl (findRenderer cache) (mempty, mempty) $ map snd comp
        stale = cache `IM.difference` found

    -- Clean the stale renderers
    sequence_ $ fmap clean stale

    -- Get the missing renderers
    new <- foldM (getElementRenderer rez) mempty $ IM.elems missing

    let next = IM.union found new
    -- Render the composite
    mapM_ (uncurry $ renderElement next) comp
    return next
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
-- | Any Element is a composite of itself if its transform type is a monoid.
instance Monoid t => Composite (Element m r t) m r t where
    composite e = [(mempty, e)]

-- | A tuple is a composite if its right type is a composite and the
-- left type is the transform and the transform is a Monoid. In this case the
-- result is the right type transformed by the left type.
instance (Monoid t, Composite a m r t) => Composite (t,a) m r t where
    composite (t, a) = map (first (mappend t)) $ composite a

-- | A Maybe is a composite if its contained type is composite. The result
-- is is the composite of its contained type or an empty list.
instance Composite a m r t => Composite (Maybe a) m r t where
    composite (Just a) = composite a
    composite _ = []

-- | A list is a composite by compositing each element and concatenating
-- the result.
instance Composite a m r t => Composite [a] m r t where
    composite = concatMap composite
