{-# LANGUAGE OverlappingInstances #-}
module Templates.Fields (
    Fields
  , runFields
  , value
  , valueM
  , object
  , objects
  ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Strict
import Text.StringTemplate.Base hiding (ToSElem, toSElem, render)
import Text.StringTemplate.Classes hiding (ToSElem, toSElem)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as M
import qualified Text.StringTemplate.Classes as HST

import Templates.TemplatesLoader ()

newtype Fields m a = Fields (StateT [(String, SElem String)] m a)
  deriving (Applicative, Functor, Monad, MonadTrans)

runFields :: Monad m => Fields m () -> m [(String, SElem String)]
runFields (Fields f) = execStateT f []

value :: (Monad m, ToSElem a) => String -> a -> Fields m ()
value name val = Fields $ modify ((name, toSElem val) :)

valueM :: (Monad m, ToSElem a) => String -> m a -> Fields m ()
valueM name mval = lift mval >>= value name

object :: Monad m => String -> Fields m () -> Fields m ()
object name obj = Fields $ do
  val <- M.fromList `liftM` lift (runFields obj)
  modify ((name, toSElem val) :)

objects :: Monad m => String -> [Fields m ()] -> Fields m ()
objects name objs = Fields $ do
  vals <- mapM (liftM M.fromList . lift . runFields) objs
  modify ((name, toSElem vals) :)

-- | Important Util. We overide default serialisation to support serialisation of bytestrings .
-- | We use ByteString with UTF all the time but default is Latin-1 and we get strange chars
-- | after rendering. !This will not always work with advanced structures.! So always convert to String.

class ToSElem a where
  toSElem :: (Stringable b) => a -> SElem b

instance (HST.ToSElem a) => ToSElem a where
  toSElem = HST.toSElem

instance ToSElem BS.ByteString where
  toSElem = HST.toSElem . BS.toString

instance ToSElem (Maybe BS.ByteString) where
  toSElem = HST.toSElem . fmap BS.toString

instance ToSElem [BS.ByteString] where
  toSElem = toSElem . fmap BS.toString

instance ToSElem String where
  toSElem l = HST.toSElem l

instance (HST.ToSElem a) => ToSElem [a] where
  toSElem l  = LI $ map HST.toSElem l

instance (HST.ToSElem a) => ToSElem (M.Map String a) where
  toSElem m = SM $ M.map HST.toSElem m
