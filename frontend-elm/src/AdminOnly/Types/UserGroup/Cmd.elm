module AdminOnly.Types.UserGroup.Cmd exposing
    ( CreateUserGroupType(..)
    , UserGroupCreated
    , UserGroupCreatedSuccess
    , createUserGroup
    , getUserGroup
    , getUserGroupStructure
    )

import AdminOnly.Types.UserGroup as UserGroup exposing (UserGroup)
import AdminOnly.Types.UserGroup.Structure as Structure exposing (UserGroupStructure(..))
import AdminOnly.UserGroupAdmin.Subscription exposing (PaymentPlan(..), enumPaymentPlan)
import Either exposing (Either)
import EnumExtra as Enum
import Http exposing (Error)
import Json.Decode as D exposing (Decoder)
import Result
import Utils exposing (Globals, formBody)


getUserGroup : (Result Error UserGroup -> msg) -> String -> Cmd msg
getUserGroup msg ugid =
    Http.get
        { url = "/adminonly/companyadmin/details/" ++ ugid
        , expect = Http.expectJson msg UserGroup.decoder
        }


getUserGroupStructure : (Result Error UserGroupStructure -> msg) -> String -> Cmd msg
getUserGroupStructure msg ugid =
    Http.get
        { url = "/adminonly/companyadmin/getstructure/" ++ ugid
        , expect =
            Http.expectJson msg
                (D.field "user_group_structure" Structure.decoder)
        }


type CreateUserGroupType
    = CreateFreeUserGroup
    | CreatePaidUserGroup
    | CreateChildUserGroup String
    | CreateRootUserGroup String


type alias UserGroupCreated =
    Result String UserGroupCreatedSuccess


type alias UserGroupCreatedSuccess =
    { userGroupId : String
    , mRootUserGroupId : Maybe String
    }


createUserGroup : (Result Error UserGroupCreated -> msg) -> Globals msg -> String -> CreateUserGroupType -> Cmd msg
createUserGroup embed globals name ugType =
    let
        params =
            case ugType of
                CreatePaidUserGroup ->
                    [ ( "payment_plan", Enum.toString enumPaymentPlan Enterprise ) ]

                CreateFreeUserGroup ->
                    [ ( "payment_plan", Enum.toString enumPaymentPlan Free ) ]

                CreateChildUserGroup ugid ->
                    [ ( "user_group_parent_id", ugid ) ]

                CreateRootUserGroup ugid ->
                    [ ( "user_group_child_id", ugid ), ( "payment_plan", Enum.toString enumPaymentPlan Enterprise ) ]
    in
    Http.post
        { url = "/adminonly/companyadmin"
        , body =
            formBody globals <|
                [ ( "user_group_name", name )
                ]
                    ++ params
        , expect = Http.expectJson embed userGroupCreatedDecoder
        }


userGroupCreatedDecoder : Decoder UserGroupCreated
userGroupCreatedDecoder =
    let
        mkResponse success =
            if success then
                D.map Ok <|
                    D.map2 UserGroupCreatedSuccess
                        (D.field "user_group_id" D.string)
                        (D.maybe <| D.field "root_user_group_id" D.string)

            else
                D.map Err <| D.field "error_message" D.string
    in
    D.field "success" D.bool
        |> D.andThen mkResponse
