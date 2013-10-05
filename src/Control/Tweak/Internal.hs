{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Control.Tweak.Internal where
import Control.Tweak.Var
import Control.Tweak.Tweakable
import Control.Concurrent.STM
import Control.Applicative

-- | 'Maker' is the 'Applicative' used to create 'Tweakable' expressions
--  Use the 'Applicative' interface or the 'Applicative' helpers
-- '.$.' and '.*.'
data Maker a = Maker { runmaker :: IO (Tweakable a) }

-- | Turn a 'Tweakable' into a 'Maker' so it can be combined
--   with other 'Maker's
make :: Tweakable a -> Maker a
make = Maker . pure

instance Functor Maker where
   fmap f (Maker mx) = Maker $ do
      af <- Pure <$> newVar f
      apply (return af) mx

instance Applicative Maker where
   pure = Maker . fmap Pure . newVar

   Maker mf <*> Maker mx = Maker $ do
      apply mf mx

-- This is workhorse or both fmap and ap
apply :: IO (Tweakable (a -> b)) -> IO (Tweakable a) -> IO (Tweakable b)
apply mf mx = do 
   -- get the Tweakables out of IO 
   x <- mx
   f <- mf
   
   -- readCacheuate them and apply the result
   let evalApp = readCacheSTM f <*> readCacheSTM x
   -- make the new var with the result of the args applied
   c <- atomically $ newVarSTM =<< evalApp

   -- the function to call on an update
   let updater = writeVarSTM c =<< evalApp

   -- add the update respectively
   addChild x (AnyVar c) updater
   addChild f (AnyVar c) updater

   return $ App c f x


-- TODO make something like applicative that works for either 

class Funktor g f where
   fcrap :: (a -> b) -> f a -> g b

instance Funktor Maker Var where
   fcrap f = fcrap f . Pure

instance Funktor Maker Tweakable where
   fcrap f = fcrap f . make 

instance Funktor Maker Maker where
   fcrap = fmap

infixl 4 .$.
-- | This is slight variation on '<$>'. Use '.$.' and '.*.' avoid explicit 
--  calls to 'make' and 'Pure'. 
--
-- Unlike Functor the input and output * -> * type can change. There is no reasoning
-- or laws behind it, it is just sugar.
-- 
-- The Funktor type class is closed and private. There are only instances
-- for 'Maker', 'Tweakable', and 'Var'.   
(.$.) :: Funktor g f => (a -> b) -> f a -> g b
(.$.) = fcrap

infixl 4 .*.
-- | This is slight variation on '<*>'. Use '.$.' and '.*.' avoid explicit 
--  calls to 'make' and 'Pure'. 
--
-- Unlike Apply, with Comply the input and output * -> * type can change. 
-- Like Funktor, there is no reasoning or laws behind it, it is just sugar.
-- 
-- The Comply type class is closed and private. There are only instances
-- for 'Maker', 'Tweakable', and 'Var'.   
(.*.) :: Comply g h => g (a -> b) -> h a -> g b
(.*.) = connect   

class Comply g h where
   connect :: g (a -> b) -> h a -> g b

instance Comply Maker Var where
   connect f = connect f . Pure 

instance Comply Maker Tweakable where
   connect f = connect f . make

instance Comply Maker Maker where
   connect f x = f <*> x





