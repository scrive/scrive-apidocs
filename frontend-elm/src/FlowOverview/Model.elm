module FlowOverview.Model exposing (..)

import Dict exposing (Dict)
import Http
import Json.Encode
import Lib.Components.FlashMessage as FlashMessage
import Lib.Types.FlashMessage exposing (FlashMessage(..))



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
    | EnterRejectionClicked
    | UpdateTextarea String
    | RejectButtonClicked
    | CancelButtonClicked
    | RejectCallback (Result Http.Error ())
    | AddFlashMessage FlashMessage
    | ErrorTrace (List ( String, Json.Encode.Value ))
    | FlashMessageMsg FlashMessage.Msg


type alias Model =
    { flashMessages : FlashMessage.State
    , state : State
    }


type State
    = Failure String
    | AppOk { flags : Flags, innerModel : InnerModel }


type alias InnerModel =
    { mInstance : Maybe GetInstanceView
    , mDocuments : Maybe (Dict String Document) -- (Dict DocumentID Document)
    , mRejection : Maybe Rejection
    }


type Rejection
    = Rejection { message : String }


type alias GetInstanceView =
    { id : String
    , state : InstanceUserState
    , actions : List InstanceUserAction
    , status : Status
    }


type Status
    = InProgress
    | Completed
    | Failed


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
