{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ExistentialQuantification #-}
{- | 'Var' is the reference type used for incremental computing. It has a cached value 
     and a list of dependent children to update when it changes.
     
     The update propogation happens automatically when using either 'modifyVar' or 
     'writeVar'. Same with the 'STM' variants. 
     
     Additionally updates can be triggered manually with 'update'
     
     'Var' is low level and is used by 'Tweakable' and to create incremental expressions.
-}
module Control.Tweak.Var
   ( -- * Reference for Incremental Computing
     Var (..)
     -- ** Existential Var Wrapper
   , AnyVar (..)
      -- ** Helpers
   , Update
   , Children
   , Cacheable (..)
     -- * Lenses
   , output
   , identifier
     -- * Dependency Manipulation
   , update
   , addChild
   , addChildSTM
     -- * Var IO CRU
   , newVar
   , readVar
   , modifyVar
   , writeVar
     -- * Var STM CRU
   , newVarSTM
   , readVarSTM
   , modifyVarSTM
   , writeVarSTM
   ) where
import Control.Concurrent.STM
import Control.Lens hiding (children)
import Control.Applicative 
import Data.Map (Map)
import qualified Data.Map as M
import Data.UniqueSTM

class Render a where
   render :: a -> IO String

-- | The type of update actions
type Update   = STM ()
-- | The container for a 'Var's dependent 'Var's.
type Children = Map AnyVar Update

-- | This a reference for incremental computation. Not only does it include a value,
--   But is also has a list of actions to execute when it is updated.
data Var a = Var 
   { _output     :: TVar a
   -- ^ The cached value of the the 'Var'
   , _children   :: TVar Children
   -- ^ A collection of actions to execute when the value of the 'Var' is updated
   , _identifier :: Unique
   -- ^ This is so to references to the same 'Var' are not added to '_children'
   --   collection
   }

-- | Just checks pointer equality not value equality
instance Eq (Var a) where
   (==) = varEq
   
varEq :: Var a -> Var b -> Bool   
varEq (Var _ _ x) (Var _ _ y) = x == y
   
instance Ord (Var a) where
   compare = varCompare
   
varCompare :: Var a -> Var b -> Ordering
varCompare (Var _ _ x) (Var _ _ y) = compare x y

instance Show a => Render (Var a) where
   render Var {..} = fmap show . atomically . readTVar $ _output

-- | a 'Lens' for the cached ref
output :: Lens (Var a) (Var b) (TVar a) (TVar b)
output = lens _output (\x y -> x {_output = y})

-- | a 'Lens' for the unique identifier associated with this 'Var'
identifier :: Lens (Var a) (Var a) Unique Unique
identifier = lens _identifier (\x y -> x { _identifier = y })

-- An existential wrapper for a 'Var'. This is useful when we need a list of 
-- 'Var's
data AnyVar = forall a. AnyVar (Var a)

instance Eq AnyVar where
   AnyVar x == AnyVar y = varEq x y

instance Ord AnyVar where
   compare (AnyVar x) (AnyVar y) = varCompare x y

-- A class for accessing the children of 'Var' or something that has a 'Var' 
-- inside it.
class Cacheable a where
   children :: Lens' a (TVar Children)

instance Cacheable (Var a) where
   children = lens _children (\x y -> x { _children = y })

instance Cacheable AnyVar where
   children = lens (\(AnyVar x)   -> view children x) 
                   (\(AnyVar x) y -> AnyVar $ set children y x)

-- | Recursively call update on the children of a 'Var' like thing
update :: Cacheable a => a -> STM ()
update x = do
   dict <- readTVar $ x^.children
   
   sequence_ . M.elems $ dict
   mapM_ update . M.keys $ dict

-- | Create a new 'Var'. See 'newVarSTM' for the 'STM' version.
newVar :: a -> IO (Var a)
newVar = atomically . newVarSTM

-- | Create a new 'Var'. See 'newVar' for the 'IO' version.
newVarSTM :: a -> STM (Var a)
newVarSTM x = Var <$> newTVar x <*> newTVar M.empty <*> newUniqueSTM

-- | Read the cached value of a 'Var'. See 'readVarSTM' for an 'STM' version
readVar :: Var a -> IO a
readVar = atomically . readVarSTM

-- | Read the cached value of a 'Var'. See 'readVar' for an 'IO' version
readVarSTM :: Var a -> STM a
readVarSTM = readTVar . view output 

-- | Modify a 'Var' and update the children. 
--   See 'modifyVar' for the 'IO' version
modifyVarSTM ::  Var a -> (a -> a) -> STM ()
modifyVarSTM var@(Var v _ _) f = do 
    modifyTVar v f 
    update var

-- | Modify a 'Var' and update the children. 
--   See 'modifyVarSTM' for the 'STM' version
modifyVar :: Var a -> (a -> a) -> IO ()
modifyVar v = atomically . modifyVarSTM v

-- | Write a new value into a 'Var' and update all of the children. 
--   See 'writeVar' for the 'IO' version
writeVarSTM :: Var a -> a -> STM ()
writeVarSTM v = modifyVarSTM v . const

-- | Write a new value into a 'Var' and update all of the children. 
--   See 'writeVarSTM' for the 'STM' version
writeVar :: Var a -> a -> IO ()
writeVar v = modifyVar v . const

-- | Add a dependent child to the 'Var's children, or any type that has 'Var' like
--   Children
--   See 'addChildSTM' for the 'STM' version
addChild :: Cacheable a 
         => a 
         -- ^ The input that contains the reference to add children to
         -> AnyVar 
         -- ^ The child 'Var' existentially wrapped up. The important info here
         --   is the unique 'Var' id.
         -> Update 
         -- ^ The update action to call when the input is updated.
         -> IO ()
addChild x k = atomically . addChildSTM x k

-- | Add a dependent child to the 'Var's children, or any type that has 'Var' like
--   Children
--   See 'addChild' for the 'IO' version
addChildSTM :: Cacheable a 
            => a 
            -- ^ The input that contains the reference to add children to
            -> AnyVar 
            -- ^ The child 'Var' existentially wrapped up. The important info here
            --   is the unique 'Var' id.
            -> Update 
            -- ^ The update action to call when the input is updated.
            -> STM ()
addChildSTM x k = modifyTVar (x^.children) . M.insert k



