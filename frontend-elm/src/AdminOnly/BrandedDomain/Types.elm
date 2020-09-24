module AdminOnly.BrandedDomain.Types exposing (..)

import Color exposing (Color)
import EnumExtra as Enum exposing (Enum)
import Lib.Types.Theme exposing (ThemeID)

type ColorIdentifier = ParticipantColor1 | ParticipantColor2 | ParticipantColor3
  | ParticipantColor4 | ParticipantColor5 | ParticipantColor6 | DraftColor
  | CancelledColor | InitiatedColor | SentColor | DeliveredColor | OpenedColor
  | ReviewedColor | SignedColor

enumColorIdentifier : Enum ColorIdentifier
enumColorIdentifier =
  let allValues = [ ParticipantColor1, ParticipantColor2, ParticipantColor3
        , ParticipantColor4, ParticipantColor5, ParticipantColor6, DraftColor
        , CancelledColor, InitiatedColor, SentColor, DeliveredColor, OpenedColor
        , ReviewedColor, SignedColor ]
      -- chosen to coincide with JSON names
      toString i = case i of
        ParticipantColor1 -> "participantColor1"
        ParticipantColor2 -> "participantColor2"
        ParticipantColor3 -> "participantColor3"
        ParticipantColor4 -> "participantColor4"
        ParticipantColor5 -> "participantColor5"
        ParticipantColor6 -> "participantColor6"
        DraftColor -> "draftColor"
        CancelledColor -> "cancelledColor"
        InitiatedColor -> "initatedColor"  -- sic
        SentColor -> "sentColor"
        DeliveredColor -> "deliveredColor"
        OpenedColor -> "openedColor"
        ReviewedColor -> "reviewedColor"
        SignedColor -> "signedColor"
      toHumanString i = case i of
        ParticipantColor1 -> "Participant 1"
        ParticipantColor2 -> "Participant 2"
        ParticipantColor3 -> "Participant 3"
        ParticipantColor4 -> "Participant 4"
        ParticipantColor5 -> "Participant 5"
        ParticipantColor6 -> "Participant 6"
        DraftColor -> "Draft, template"
        CancelledColor -> "Error, withdrawn, cancelled, timed-out"
        InitiatedColor -> "Initiated"
        SentColor -> "Sent"
        DeliveredColor -> "Delivered"
        OpenedColor -> "Email opened, prolonged"
        ReviewedColor -> "Reviewed online, opened view to identify, identified online"
        SignedColor -> "Signed, sealed"
  in Enum.makeEnum allValues toString toHumanString


type ThemeKind = EmailTheme | SignViewTheme | ServiceTheme | LoginTheme

enumThemeKind : Enum ThemeKind
enumThemeKind =
  let allValues = [ EmailTheme, SignViewTheme, ServiceTheme, LoginTheme ]
      -- chosen to coincide with JSON names
      toString k = case k of
        EmailTheme -> "mailTheme"
        SignViewTheme -> "signviewTheme"
        ServiceTheme -> "serviceTheme"
        LoginTheme -> "loginTheme"
      toHumanString k = case k of
        EmailTheme -> "Email"
        SignViewTheme -> "Sign View"
        ServiceTheme -> "Service"
        LoginTheme -> "Login"
  in Enum.makeEnum allValues toString toHumanString

type alias BrandedDomainID = Int

type alias BrandedDomain =
    { id : BrandedDomainID
    , mainDomain : Bool
    , url : String
    , smsOriginator : String
    , emailOriginator : String
    , themes : Enum.Dict ThemeKind ThemeID
    , browserTitle : String
    , favicon : String
    , colors : Enum.Dict ColorIdentifier Color
    }

defaultBrandedDomain : BrandedDomain
defaultBrandedDomain =
  { id = -1
  , mainDomain = False
  , url = ""
  , smsOriginator = ""
  , emailOriginator = ""
  , themes = Enum.empty enumThemeKind
  , browserTitle = ""
  , favicon = ""
  , colors = Enum.empty enumColorIdentifier
  }
