module AdminOnly.Types.UserGroup.Structure exposing
    ( UserGroupNode
    , UserGroupStructure(..)
    , decoder
    , getRootUserGroup
    , lookupUserGroup
    )

import Json.Decode as D exposing (Decoder)
import List as L


type UserGroupStructure
    = UserGroupStructure UserGroupNode (List UserGroupStructure)


type alias UserGroupNode =
    { name : String
    , ugid : String
    , isBillable : Bool
    }


getRootUserGroup : UserGroupStructure -> UserGroupNode
getRootUserGroup (UserGroupStructure root _) =
    root


lookupUserGroup : String -> UserGroupStructure -> Maybe UserGroupNode
lookupUserGroup ugid (UserGroupStructure node children) =
    if node.ugid == ugid then
        Just node

    else
        L.head <| L.filterMap (lookupUserGroup ugid) children


decoder : Decoder UserGroupStructure
decoder =
    D.map2 UserGroupStructure
        (D.field "group" <|
            D.map3 UserGroupNode
                (D.field "name" D.string)
                (D.field "user_group_id" D.string)
                (D.field "is_billable" D.bool)
        )
        (D.field "children" <| D.list <| D.lazy (\_ -> decoder))
