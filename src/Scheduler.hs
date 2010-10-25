{-# LANGUAGE TemplateHaskell, UndecidableInstances, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
-- |Simple session support
module Scheduler
    ( runScheduler )
    where

import System.IO
import DocState
import Happstack.State (query, update)
import Control.Monad
import MinutesTime
import System.Log.Logger

runScheduler = do
                timeoutDocuments 
                debugM "Happstack.Server" $ "Scheduler is running ..."                                                        

timeoutDocuments = do
                    now <- getMinutesTime
                    docs <- query $ GetTimeoutedButPendingDocuments now
                    forM_ docs $ \doc -> do 
                                           update $ UpdateDocumentStatus (documentid doc) Timedout now 0                                                     
                                           debugM "Happstack.Server" $ "Document timedout" ++ (show $ documenttitle doc)                                                                   
                                          
                                           