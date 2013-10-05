{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-| Tweakable expressions can be updated incrementally. This module
    exports the central expression type for incremental computations.
    However, the 'Tweakable' expressions are meant to be created with 
    'Maker'. 
   
    Use 'readCache' to retrieve the value of an 'Tweakable' at anytime.
-}
module Control.Tweak.Tweakable 
   ( -- * The Main Expression Type
     Tweakable (..)
     -- * Accessors 
   , getVar
   , getAnyVar
   , readCache
   , readCacheSTM
   ) where
import Control.Concurrent.STM
import Control.Tweak.Var
import Control.Lens hiding (children, to, from)

-- | An expression that can be incrementally updated. 
--   'Tweakable' is basically an simple 'Applicative'
--   with a cached value.  
data Tweakable a where
   App   :: Var b -> Tweakable (a -> b) -> Tweakable a -> Tweakable b
   Pure  :: Var a -> Tweakable a

-- | Get the cache i.e. the 'Var' from the Tweakable
getVar :: Tweakable a -> Var a
getVar = \case
   Pure x     -> x
   App  x _ _ -> x

-- | Get the cache and put in an 'AnyVar' existential wrapper
getAnyVar :: Tweakable a -> AnyVar
getAnyVar = AnyVar . getVar

instance Cacheable (Tweakable a) where
   children = lens to from where
      to = \case
         Pure c     -> view children c
         App  c _ _ -> view children c

      from x y = case x of
         Pure c     -> Pure (set children y c)
         App  c a b -> App  (set children y c) a b

-- | Read the cache of a 'Tweakable'. This is nothing more than 
-- 
-- @ 
--   readCache = atomically . readCacheSTM  
-- @
readCache :: Tweakable a -> IO a
readCache = atomically . readCacheSTM

-- | Read the cache of a 'Tweakable'. See 'readCache' for an IO version.
readCacheSTM :: Tweakable a -> STM a
readCacheSTM = readTVar . view output . getVar
