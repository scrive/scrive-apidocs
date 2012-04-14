module Util.FinishWith where

import Control.Monad.IO.Class
import Data.Typeable
import Happstack.Server hiding (finishWith)
import qualified Control.Exception as E

import Context
import KontraMonad

-- | We don't want to use finishWith from Happstack, because
-- it returns directly to Happstack, omiting our main router
-- and thus not giving us a chance to update context if necessary
-- and clean up after ourselves (close db connection etc.)

data FinishWith = FinishWith Response Context
  deriving Typeable

instance Show FinishWith where
  show (FinishWith res _) = "FinishWith " ++ show res

instance E.Exception FinishWith

-- | Passing monadic action there sucks, but apparently there is no way
-- to get filter that is currently stored in underlying monad, you can
-- only get it from locally run action.
finishWith :: (FilterMonad Response m, KontraMonad m, MonadIO m) => m Response -> m a
finishWith m = do
  (res, f) <- getFilter m
  liftIO . E.throwIO . FinishWith (f res) =<< getContext
