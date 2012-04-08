 module Text.JSON.Gen (
    module Text.JSON.ToJSValue
  , JSONGen
  , runJSONGen
  , JSONGenT
  , runJSONGenT
  , value
  , object
  , objects
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

value :: (Monad m, ToJSValue a) => String -> a -> JSONGenT m ()
value name val = JSONGenT $ modify (|> (name, toJSValue val))

object :: Monad m => String -> JSONGenT m () -> JSONGenT m ()
object name json = JSONGenT $ do
  val <- lift $ runJSONGenT json
  modify (|> (name, toJSValue val))

objects :: Monad m => String -> [JSONGenT m ()] -> JSONGenT m ()
objects name jsons = JSONGenT $ do
  val <- mapM (lift . runJSONGenT) jsons
  modify (|> (name, toJSValue val))
