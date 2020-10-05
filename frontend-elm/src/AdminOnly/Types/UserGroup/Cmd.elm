module AdminOnly.Types.UserGroup.Cmd exposing (getUserGroup)

import AdminOnly.Types.UserGroup as UserGroup exposing (UserGroup)
import Http exposing (Error)
import Result


getUserGroup : (Result Error UserGroup -> msg) -> String -> Cmd msg
getUserGroup msg ugid =
    Http.get
        { url = "/adminonly/companyadmin/details/" ++ ugid
        , expect = Http.expectJson msg UserGroup.decoder
        }
