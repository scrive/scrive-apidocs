module IdentifyView.FITupasNets.Tupas exposing (..)

import Enum exposing (Enum)

type NetsErrorMessage = None | AuthFailed | Cancel | NoBrowser | NoCookies
  | NoJava | NoJavaScript | NoOS | OldOS | OldJava | OldJS | UnsupportedVersion
  | UnsupportedCharset | Blocked | Revoked | Expired

enumNetsErrorMessage : Enum NetsErrorMessage
enumNetsErrorMessage =
  let allValues = [ None , AuthFailed , Cancel , NoBrowser , NoCookies, NoJava
        , NoJavaScript , NoOS , OldOS , OldJava , OldJS , UnsupportedVersion
        , UnsupportedCharset , Blocked , Revoked , Expired ]

      toString err = case err of
        None -> "identify_none"
        AuthFailed -> "identify_authfailed"
        Cancel -> "identify_cancel"
        NoBrowser -> "identify_ua.nobrowser"
        NoCookies -> "identify_ua.nocookies"
        NoJava -> "identify_ua.nojava"
        NoJavaScript -> "identify_ua.nojavascript"
        NoOS -> "identify_ua.noos"
        OldOS -> "identify_ua.oldos"
        OldJava -> "identify_ua.oldjava"
        OldJS -> "identify_ua.oldjs"
        UnsupportedVersion -> "identify_ua.unsupported.version"
        UnsupportedCharset -> "identify_ua.unsupported.charset"
        Blocked -> "identify_uid.blocked"
        Revoked -> "identify_uid.revoked"
        Expired -> "identify_uid.expired"
  in Enum.makeEnum allValues toString

-- Adapted from `asValidFinnishSSN` (InputValidation.hs).
isPersonalNumberValid : String -> Bool
isPersonalNumberValid personalNumber =
  case String.toList <| String.toUpper <| String.filter (\c -> c /= ' ') personalNumber of
  [d1, d2, m1, m2, y1, y2, sep, x1, x2, x3, checksum] ->

    let mday = String.toInt <| String.fromList [d1,d2]
        mmonth = String.toInt <| String.fromList [m1,m2]
        mcombined_digits = String.toInt <| String.fromList [d1, d2, m1, m2, y1, y2, x1, x2, x3]

    in case (mday, mmonth, mcombined_digits) of
    (Just day, Just month, Just combined_digits) ->

      let checksum_chars = "0123456789ABCDEFHJKLMNPRSTUVWXY"
          computed_checksum = modBy (String.length checksum_chars) combined_digits
          checksum_matches =
            [computed_checksum] == String.indices (String.fromChar checksum) checksum_chars

      in 1 <= day && day <= 31
         && 1 <= month && month <= 12
         && List.member sep ['-','+','A']
         && checksum_matches

    _ -> False
  _ -> False
