module ELegitimation.Routes (handleRoutes) where

import ELegitimation.BankID as BankID
import Kontra(Kontra)
import Happstack.StaticRouting (Route, dir, param, choice)
import Routing (hPostNoXToken, hGet, hPost, toK1, toK2, toK3, toK4)
import Happstack.Server (Response)

handleRoutes :: Route (Kontra Response)
handleRoutes = choice
     [
     -- I put this stuff up here because someone changed things out from under me
     -- I will rearrange this later
       dir "s" $ hGet                         $ toK4 $ BankID.handleSignBankID
     , dir "s" $ param "eleg" $ hPostNoXToken $ toK3 $ BankID.handleSignPostBankID
     , dir "s" $ param "eleg" $ hPost         $ toK3 $ BankID.handleSignPostBankID -- FIXME: Why is this needed?
     , dir "d" $ hGet                         $ toK2 $ BankID.handleIssueBankID
     , dir "d" $ param "eleg" $ hPost         $ toK1 $ BankID.handleIssuePostBankID
     ]


