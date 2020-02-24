module Component.BrandedDomain.Data exposing (ActionColors, Branding, ParticipantColors, ThemeSet)

import Color exposing (Color)
import Component.Theme.Data exposing (Theme)


type alias ThemeSet =
    { emailTheme : Theme
    , signViewTheme : Theme
    , serviceTheme : Theme
    , loginTheme : Theme
    }


type alias ActionColors =
    { sentColor : Color
    , draftColor : Color
    , signedColor : Color
    , openedColor : Color
    , reviewedColor : Color
    , initatedColor : Color
    , cancelledColor : Color
    , deliveredColor : Color
    }


type alias ParticipantColors =
    { participantColor1 : Color
    , participantColor2 : Color
    , participantColor3 : Color
    , participantColor4 : Color
    , participantColor5 : Color
    , participantColor6 : Color
    }


type alias Branding =
    { brandedDomainId : String
    , browserTitle : String
    , mainDomain : Bool
    , url : String
    , favicon : String
    , emailOriginator : String
    , smsOriginator : String
    , themeIds :
        { emailTheme : String
        , signViewTheme : String
        , serviceTheme : String
        , loginTheme : String
        }
    , brandingColors : ActionColors
    , participantColors : ParticipantColors
    }
