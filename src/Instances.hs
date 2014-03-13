{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Instances for lifting various monad combinations
module Instances() where

import Amazon (AmazonMonad)
import Crypto.RNG (CryptoRNG)
import DB (MonadDB)
import DB.RowCache (RowCacheT(..))
import GuardTime (GuardTimeConfT(..), GuardTimeConfMonad)
import Happstack.Server (FilterMonad, HasRqData, ServerMonad, WebMonad)
import Kontra (Kontrakcja)
import KontraMonad (KontraMonad)
import Log (MonadLog)
import MailContext (MailContextT(..), MailContextMonad)
import Text.StringTemplates.Templates (TemplatesMonad, TemplatesT(..))
import Text.StringTemplates.Fields (Fields(..))

deriving instance (Monad m, AmazonMonad m) => AmazonMonad (MailContextT m)
deriving instance (MonadLog m) => MonadLog (MailContextT m)
deriving instance (MonadDB m) => MonadDB (MailContextT m)
deriving instance (CryptoRNG m) => CryptoRNG (MailContextT m)
deriving instance (TemplatesMonad m) => TemplatesMonad (MailContextT m)

deriving instance (Monad m, AmazonMonad m) => AmazonMonad (GuardTimeConfT m)
deriving instance (MonadLog m) => MonadLog (GuardTimeConfT m)
deriving instance (MonadDB m) => MonadDB (GuardTimeConfT m)
deriving instance (CryptoRNG m) => CryptoRNG (GuardTimeConfT m)
deriving instance (TemplatesMonad m) => TemplatesMonad (GuardTimeConfT m)
deriving instance MailContextMonad m => MailContextMonad (GuardTimeConfT m)
deriving instance MailContextMonad m => MailContextMonad (Fields m)

deriving instance (Monad m, AmazonMonad m) => AmazonMonad (TemplatesT m)

deriving instance (Monad m, AmazonMonad m) => AmazonMonad (RowCacheT r m)
deriving instance (Monad m, MailContextMonad m) => MailContextMonad (RowCacheT r m)
deriving instance (Monad m, GuardTimeConfMonad m) => GuardTimeConfMonad (RowCacheT r m)
deriving instance (Monad m, TemplatesMonad m) => TemplatesMonad (RowCacheT r m)
deriving instance (Monad m, MonadDB m) => MonadDB (RowCacheT r m)
deriving instance (Monad m, MonadLog m) => MonadLog (RowCacheT r m)
deriving instance (Monad m, CryptoRNG m) => CryptoRNG (RowCacheT r m)
deriving instance KontraMonad m => KontraMonad (RowCacheT r m)
deriving instance FilterMonad f m => FilterMonad f (RowCacheT r m)
deriving instance (Monad m, HasRqData m) => HasRqData (RowCacheT r m)
deriving instance ServerMonad m => ServerMonad (RowCacheT r m)
deriving instance WebMonad a m => WebMonad a (RowCacheT r m)

instance Kontrakcja m => Kontrakcja (RowCacheT r m)
