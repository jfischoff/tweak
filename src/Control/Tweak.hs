{- | Tweak exposes an interface for incremental computation. 
   
   There are three main types that work together to build expressions that can 
   updated incrementally: 'Maker', 'Tweakable', and 'Var'.
   
   'Maker' exposes a 'Functor' and 'Applicative' interface for building 'Tweakable' 
   expressions
   
   'Tweakable' constructs expressions that can be re-evaluated incrementally by calling
   'readCache'. In contains caches of 'Var's
   
   'Var' is a mututable reference with dependency information, which propagates changes
   through a graph of 'Var's.
   
   Under the hood everything is done using 'TVar's and the system is meant to be used
   in a concurrent environment. There are 'STM' versions of functions that occur in 'IO' 
   to help build more complex 'STM' transactions.
   
   Here is a simple example.
   
   
   > Import Control.Tweak
   > 
   > test = do
   >      foo <- newVar 1
   >      baz <- newVar 2
   > 
   >      quux <- runMaker $ (*) <$> make baz <*> make baz
   >      bar  <- runMaker $ (+) <$> make foo <*> make quux
   >      
   >      -- prints 5
   >      print =<< readCache bar
   >
   >      writeVar foo 10
   >      --
   >      print =<< readCache bar        
   
   
   
   It is a little inconvient to explictly convert 'Var's into 'Maker's so there is some 
   'Applicative' and 'Functor' like sugar for '<$>' and '<*>', that also does the 
   proper wrapping of 'Var' and 'Tweakable'.
   
   Using the sugar the example above looks like. 
   
   > Import Control.Tweak
   > 
   > test = do
   >      foo <- newVar 1
   >      baz <- newVar 2
   > 
   >      quux <- runMaker $ (*) .$. baz .*. baz
   >      bar  <- runMaker $ (+) .$. foo .*. quux
   >      
   >      -- prints 5
   >      print =<< readCache bar
   >
   >      writeVar foo 10
   >      --
   >      print =<< readCache bar
   
   The important people of the example above, is when the @foo@ is updated, only @bar@
   is updated, not @quux@
   
-}
module Control.Tweak 
   ( 
   -- * Maker Interface
     Maker
   , runmaker
   , make
   , (.$.)
   , (.*.)
   , module Control.Applicative
   -- * Tweakable Interface
   , Tweakable (..)
   , readCache
   , readCacheSTM
   -- * Var interface
   , Var
   -- ** IO Var CRU
   , newVar
   , modifyVar
   , writeVar
   , readVar
   -- ** STM Var CRU
   , newVarSTM
   , modifyVarSTM
   , writeVarSTM
   , readVarSTM
   ) where
import Control.Tweak.Internal
import Control.Tweak.Tweakable
import Control.Tweak.Var
import Control.Applicative