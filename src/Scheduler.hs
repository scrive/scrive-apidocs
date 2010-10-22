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
                    docs <- query GetDocuments      
                    now <- getMinutesTime
                    forM_ docs $ \doc -> do                     
                                           case (documenttimeouttime doc) of
                                            Just timeout -> do
                                                             let pending = (documentstatus)  doc == Pending 
                                                             let expired = (unTimeoutTime timeout) < now
                                                             if (pending && expired)
                                                              then do 
                                                                    update $ UpdateDocumentStatus doc Timedout now 0                                                     
                                                                    debugM "Happstack.Server" $ "Document timedout" ++ (show $ documenttitle doc)                                                                   
                                                              else return ()
                                            _ -> return () 
                                          
                                           