module ELegitimation.Routes(handleRoutes) where

import ELegitimation.BankID as BankID
import Kontra(Kontra, param)
import Routing(hGet2, hGet4, hPost1, hPost3, toK1, toK2, toK3, toK4, hPostNoXToken3)
import Happstack.Server(Response, dir)

handleRoutes :: [Kontra Response]
handleRoutes = 
     [
     -- I put this stuff up here because someone changed things out from under me
     -- I will rearrange this later
       dir "s" $ hGet4                        $ toK4 $ BankID.handleSignBankID
     , dir "s" $ param "eleg" $ hPostNoXToken3 $ toK3 $ BankID.handleSignPostBankID
     , dir "d" $ hGet2                        $ toK2 $ BankID.handleIssueBankID
     , dir "d" $ param "eleg" $ hPost1        $ toK1 $ BankID.handleIssuePostBankID
     , dir "s" $ hGet4  $ toK4 $ BankID.handleSignBankID
     , dir "s" $ param "eleg" $ hPost3 $ toK3 $ BankID.handleSignPostBankID
     , dir "d" $ hGet2  $ toK2 $ BankID.handleIssueBankID
     , dir "d" $ param "eleg" $ hPost1 $ toK1 $ BankID.handleIssuePostBankID
     ]


