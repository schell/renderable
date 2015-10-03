{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Renderable where

import Prelude hiding (lookup)
import Control.Monad
import Data.Hashable
import Data.IntMap (IntMap)
import Data.Maybe
import Data.Monoid
import Data.List (intercalate)
import qualified Data.IntSet as S
import qualified Data.IntMap as IM
import GHC.Stack

--------------------------------------------------------------------------------
-- Decomposable Instances
--------------------------------------------------------------------------------
-- | Any element is decomposable by returning a list consisting of itself.
instance Decomposable (Element m r t) m r t where
    decompose e = [e]
--------------------------------------------------------------------------------
-- Renderable Instances
--------------------------------------------------------------------------------
-- | Any Element is renderable by rendering its contained datatype.
instance Renderable (Element m r t) where
    type RenderMonad (Element m r t) = m
    type RenderRsrc (Element m r t) = r
    type RenderTfrm (Element m r t) = t
    cache rz rs (Element a)   = attachIfNeeded rz rs a
    nameOf (Element a)        = "Element " ++ nameOf a
    composite (Element a) = composite a

-- | A tuple is renderable when it is a pairing of a transform and another
-- renderable datatype.
instance ( t ~ RenderTfrm a, Show t, Monoid t
         , Hashable a, Renderable a) => Renderable (t,a) where
    type RenderMonad (t,a) = RenderMonad a
    type RenderTfrm (t,a) = RenderTfrm a
    type RenderRsrc (t,a) = RenderRsrc a
    cache rz rs (_,a) = attachIfNeeded rz rs a
    nameOf (t,a) = "(" ++ show t ++ ", " ++ nameOf a ++ ")"
    composite (t,a) = map (fmap $ fmap (t <>)) $ composite a

-- | A Maybe is renderable by rendering the datatype contained in the Just
-- constructor or by rendering nothing.
instance (Renderable a, Hashable a, Show a) => Renderable (Maybe a) where
    type RenderMonad (Maybe a) = RenderMonad a
    type RenderTfrm (Maybe a) = RenderTfrm a
    type RenderRsrc (Maybe a) = RenderRsrc a
    cache rz rs (Just a) = attachIfNeeded rz rs a
    cache _ rs _         = return rs
    nameOf (Just a) = "Just " ++ nameOf a
    nameOf _        = "Nothing"
    composite (Just a) = composite a
    composite _ = []

-- | A list of renderable instances is renderable by rendering each
-- instance.
instance (Renderable a, Hashable a) => Renderable [a] where
    type RenderMonad [a] = RenderMonad a
    type RenderTfrm [a] = RenderTfrm a
    type RenderRsrc [a] = RenderRsrc a
    cache = foldM . attachIfNeeded
    nameOf as = "[ " ++ (intercalate ", " names) ++ " ]"
        where names = map nameOf as
    composite = concatMap composite
--------------------------------------------------------------------------------
-- Rendering and cacheing
--------------------------------------------------------------------------------
-- | Render a datatype using renderings stored in the given cache.
renderData :: (Monad m, Renderable a, Monoid (RenderTfrm a))
           => Cache m (RenderTfrm a) -> a -> m ()
renderData c = renderComposite c mempty . composite

-- | Render only the hidden layers of a datatype using renderings stored in
-- the given cache. This is sometimes useful for debugging.
renderDataHidden :: (Renderable a, Monad m, Monoid (RenderTfrm a))
                 => Cache m (RenderTfrm a) -> (RenderTfrm a) -> a -> m ()
renderDataHidden c t = renderComposite c t . catMaybes . map f . composite
    where f (i, Nothing) = Just (i, Just mempty)
          f _ = Nothing

-- | Render the composite of a datatype using renderings stored in the
-- given cache.
renderComposite :: (Monad m, Monoid t) => Cache m t -> t -> Composite t -> m ()
renderComposite rs t = mapM_ (uncurry go)
    where go k (Just t') = maybe (err k) (rend t') $ IM.lookup k rs
          go _ _ = return ()
          rend t' (Rendering f _) = f $ t <> t'
          err k = errorWithStackTrace $ unwords [ "Fatal error! Could not find"
                                                , "rendering (from a layer)"
                                                , show k
                                                ]

-- | If needed, create a new rendering given some resources, insert it in
-- the cache and return the new cache.
attachIfNeeded :: ( Renderable a, Monad (RenderMonad a)
                 , Monoid (RenderTfrm a), Hashable a)
              => RenderRsrc a -> Cache (RenderMonad a) (RenderTfrm a)
              -> a -> (RenderMonad a) (Cache (RenderMonad a) (RenderTfrm a))
attachIfNeeded rz cache' a =
    maybe (cache rz cache' a) (const $ return cache') $ IM.lookup (hash a) cache'

-- | Detach any renderings that are not needed to render the
-- given data.
detachUnused :: (Monad m, Renderable a) => Cache m t -> a -> m (Cache m t)
detachUnused c a =
    -- Get the hashes listed in the composite (these are used)
    let hashes = S.fromList $ map fst $ composite a
        -- Get the hashes currently in the cache
        keys = IM.keysSet c
        -- Diff them
        diff = S.difference keys hashes
        -- Detach them
    in foldM detach c $ S.toList diff

-- | Remove a rendering from a cache and clean up the resources allocated
-- for that rendering.
detach :: Monad m => Cache m t -> Int -> m (Cache m t)
detach c k = do
    case IM.lookup k c of
        Nothing        -> let s = "Could not find rendering for " ++ show k
                          in errorWithStackTrace s
        Just rendering -> clean rendering
    return $ IM.delete k c
--------------------------------------------------------------------------------
-- Decomposition
--------------------------------------------------------------------------------
-- | An instance of Decomposable can be broken down into a number of elements.
class Decomposable a m r t where
    decompose :: a -> [Element m r t]
--------------------------------------------------------------------------------
-- Element
--------------------------------------------------------------------------------
instance Hashable (Element m r t) where
    hashWithSalt s (Element a) = s `hashWithSalt` "Element" `hashWithSalt` a

instance Eq (Element m r t) where
    a == b = hash a == hash b

instance Show (Element m r t) where
    show (Element a) = "Element{ " ++ show a ++ " }"

-- | Element is a generic existential type that can be used to enclose
-- instances of Renderable in order to contain them all in a heterogeneous list.
-- 'm', 'r' and 't' must be shared with all Renderable instances stored in
-- a heterogeneous list of Elements.
data Element m r t where
    Element  :: ( Monad m, Show a, Hashable a, Renderable a
                , m ~ RenderMonad a
                , r ~ RenderRsrc a
                , t ~ RenderTfrm a)
             => a -> Element m r t
--------------------------------------------------------------------------------
-- Renderable
--------------------------------------------------------------------------------
class Renderable a where
    -- | The monad needed to render the datatype.  In most cases this is
    -- probably IO.
    type RenderMonad a :: * -> *
    -- | The datatype that is used to transform renderings.
    type RenderTfrm a  :: *
    -- | The datatype that holds cached resources that will be used to
    -- composite and render the datatype.
    type RenderRsrc a  :: *
    -- | The name of a renderable datatype. This is mostly for debugging.
    nameOf :: a -> String
    -- | Store the rendering of a datatype in a cache keyed by the hash of that
    -- datatype. Returns the new cache.
    cache :: (Monad (RenderMonad a), Monoid (RenderTfrm a))
          => RenderRsrc a -> Cache (RenderMonad a) (RenderTfrm a) -> a
          -> (RenderMonad a) (Cache (RenderMonad a) (RenderTfrm a))
    -- | The entire composite list of renderings for a given datatype.
    composite :: a -> Composite (RenderTfrm a)

-- | A cache of renderings.
type Cache m t = IntMap (Rendering m t)

instance Monad m => Monoid (Rendering m t) where
    (Rendering a b) `mappend` (Rendering c d) =
        Rendering (\t -> a t >> c t) (b >> d)
    mempty = Rendering (const $ return ()) (return ())

-- | A rendering is a type that contains some effectful computation for
-- displaying something given a transform. It also contains an effectful
-- computation for cleaning up any resources allocated during its creation.
data Rendering m t = Rendering { render :: t -> m ()
                               , clean  :: m ()
                               }

-- | A composite is a representation of the entire rendered datatype. It is
-- a flattened list of all the renderings (denoted by hash), along with
-- that rendering\'s local transformation. If a rendering is explicitly run
-- by another rendering (as in a Renderable class definition) then the
-- transformation for that rendering should be Nothing, which will keep
-- 'renderComposite' from running that rendering in addition to the
-- rendering its included in. For example:
-- @
-- [(0, Just $ Transform (10,10) (0.5,0.5) 0)
-- ,(1, Nothing)
-- ]
-- @
-- The above is a composite of two renderings, the first will be rendered
-- by 'renderComposite' using the given transform while the second is
-- effectively hidden but present. Being present in the composite will keep
-- 'detachUnused' from detaching and cleaning the rendering.
type Composite a = [(Int, Maybe a)]
