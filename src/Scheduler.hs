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
import AppControl

runScheduler appconfig = 
               do
                timeoutDocuments 
                debugM "Happstack.Server" $ "Scheduler is running ..."                                                        

timeoutDocuments = do
                    now <- getMinutesTime
                    docs <- query $ GetTimeoutedButPendingDocuments now
                    forM_ docs $ \doc -> do 
                                           update $ TimeoutDocument (documentid doc) now                                                     
                                           debugM "Happstack.Server" $ "Document timedout" ++ (show $ documenttitle doc)                                                                   
                                          
