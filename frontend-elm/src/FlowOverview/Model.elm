module FlowOverview.Model exposing (..)

import Dict exposing (Dict)
import Http



-- TODO: Make IDs more type-safe


type alias Flags =
    { cookies : Dict String String
    , kontraApiUrl : String
    , flowApiUrl : String
    , flowInstanceId : String
    }


type Msg
    = GetInstanceViewReceived (Result Http.Error GetInstanceView)
    | GetDocumentReceived (Result Http.Error Document)


type Model
    = Failure String
    | AppOk { flags : Flags, innerModel : InnerModel }


type alias InnerModel =
    { mInstance : Maybe GetInstanceView
    , mDocuments : Maybe (Dict String Document) -- (Dict DocumentID Document)
    }


type alias GetInstanceView =
    { id : String
    , state : InstanceUserState
    , actions : List InstanceUserAction
    }


type alias InstanceUserState =
    { documents : List InstanceUserDocument
    }


type alias InstanceUserDocument =
    { documentId : String
    , documentState : String
    , signatoryId : String
    }


type alias InstanceUserAction =
    { actionType : String
    , actionDocument : String -- DocumentID
    , actionSignatoryId : String
    , actionLink : Url
    }


type alias Document =
    { id : String
    , title : String
    , parties : List SignatoryLink
    }


type alias SignatoryLink =
    { id : String
    , isAuthor : Bool
    , fields : List SignatoryField
    }


type SignatoryField
    = SignatoryNameField
        { nameOrder : Int
        , value : String
        }
    | SignatoryPersonalNumberField
        { value : String
        }
    | SignatoryEmailField
        { value : String
        }
    | SignatoryMobileField
        { value : String
        }
    | SignatoryOtherField
        -- I didn't bother to implement all fields
        { sfType : String
        }


type alias Url =
    String


updateModel : Model -> (InnerModel -> InnerModel) -> Model
updateModel model f =
    case model of
        AppOk { flags, innerModel } ->
            AppOk { flags = flags, innerModel = f innerModel }

        Failure _ ->
            model
