module Component.Global.Msg exposing (GlobalMsg(..), flashError, flashSuccess, initialGlobals, initialToFinalGlobal)

import Compose.Util exposing (msgToCmd)
import FlashMessage exposing (FlashMessage)
import Utils exposing (Globals)


type GlobalMsg
    = FlashMsg FlashMessage
    | SetPageUrlFromModelMsg
    | GoToUserGroupUsersMsg String
    | GoToDaveDocumentMsg String
    | GoToUserMsg String
    | GoToUserGroupMsg String
    | GoToUserGroupAdminTabMsg
    | GoToUserAdminTabMsg
    | GoToBrandedDomainMsg String
    | GoToBrandedDomainsTabMsg


initialToFinalGlobal : Globals msg -> GlobalMsg -> Cmd msg
initialToFinalGlobal globals msg1 =
    case msg1 of
        FlashMsg msg2 ->
            globals.flashMessage msg2

        SetPageUrlFromModelMsg ->
            globals.setPageUrlFromModel

        GoToUserGroupUsersMsg msg2 ->
            globals.gotoUserGroupUsers msg2

        GoToDaveDocumentMsg msg2 ->
            globals.gotoDaveDocument msg2

        GoToUserMsg msg2 ->
            globals.gotoUser msg2

        GoToUserGroupMsg msg2 ->
            globals.gotoUserGroup msg2

        GoToUserGroupAdminTabMsg ->
            globals.gotoUserGroupAdminTab

        GoToUserAdminTabMsg ->
            globals.gotoUserAdminTab

        GoToBrandedDomainMsg msg2 ->
            globals.gotoBrandedDomain msg2

        GoToBrandedDomainsTabMsg ->
            globals.gotoBrandedDomainsTab


initialGlobals : String -> String -> Globals GlobalMsg
initialGlobals xtoken cdnBaseUrl =
    { xtoken = xtoken
    , cdnBaseUrl = cdnBaseUrl
    , flashMessage =
        msgToCmd << FlashMsg
    , setPageUrlFromModel =
        msgToCmd SetPageUrlFromModelMsg
    , gotoUserGroupUsers =
        msgToCmd << GoToUserGroupUsersMsg
    , gotoDaveDocument =
        msgToCmd << GoToDaveDocumentMsg
    , gotoUser =
        msgToCmd << GoToUserMsg
    , gotoUserGroup =
        msgToCmd << GoToUserGroupMsg
    , gotoUserGroupAdminTab =
        msgToCmd GoToUserGroupAdminTabMsg
    , gotoUserAdminTab =
        msgToCmd GoToUserAdminTabMsg
    , gotoBrandedDomain =
        msgToCmd << GoToBrandedDomainMsg
    , gotoBrandedDomainsTab =
        msgToCmd GoToBrandedDomainsTabMsg
    }


flashSuccess : String -> GlobalMsg
flashSuccess =
    FlashMsg << FlashMessage.success


flashError : String -> GlobalMsg
flashError =
    FlashMsg << FlashMessage.error
