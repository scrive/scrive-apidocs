{-# LANGUAGE FunctionalDependencies, OverlappingInstances #-}
module Text.JSON.Gen (
    module Text.JSON.ToJSValue
  , JSONGen
  , runJSONGen
  , JSONGenT
  , runJSONGenT
  , field
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.State.Strict
import Data.Foldable
import Data.Sequence as S
import Text.JSON
import Text.JSON.ToJSValue

type JSONGen = JSONGenT Identity

runJSONGen :: JSONGen () -> JSValue
runJSONGen = runIdentity . runJSONGenT

newtype JSONGenT m a = JSONGenT (StateT (Seq (String, JSValue)) m a)
  deriving (Applicative, Functor, Monad, MonadTrans)

instance MonadIO m => MonadIO (JSONGenT m) where
  liftIO = JSONGenT . liftIO

runJSONGenT :: Monad m => JSONGenT m () -> m JSValue
runJSONGenT (JSONGenT f) = (JSObject . toJSObject . toList) `liftM` execStateT f S.empty

class JSField m a | a -> m where
  field :: String -> a -> JSONGenT m ()

instance (Monad m, ToJSValue a) => JSField m a where
  field name value = JSONGenT $ modify (|> (name, toJSValue value))

instance Monad m => JSField m (JSONGenT m ()) where
  field name json = JSONGenT $ do
    value <- lift $ runJSONGenT json
    modify (|> (name, toJSValue value))

instance Monad m => JSField m [JSONGenT m ()] where
  field name jsons = JSONGenT $ do
    values <- mapM (lift . runJSONGenT) jsons
    modify (|> (name, toJSValue values))
