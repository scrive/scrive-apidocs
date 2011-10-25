module ELegitimation.Routes (handleRoutes) where

import ELegitimation.BankID as BankID
import Kontra(Kontra)
import Happstack.StaticRouting (Route, dir, param, choice)
import Routing (hPostNoXToken, hGet, hPost)
import Happstack.Server (Response)

handleRoutes :: Route (Kontra Response)
handleRoutes = choice
     [
     -- I put this stuff up here because someone changed things out from under me
     -- I will rearrange this later
       dir "s" $ hGet                         $ BankID.handleSignBankID
     , dir "s" $ param "eleg" $ hPostNoXToken $ BankID.handleSignPostBankID
     , dir "s" $ param "eleg" $ hPost         $ BankID.handleSignPostBankID -- FIXME: Why is this needed?
     , dir "d" $ hGet                         $ BankID.handleIssueBankID
     , dir "d" $ param "eleg" $ hPost         $ BankID.handleIssuePostBankID
     ]


