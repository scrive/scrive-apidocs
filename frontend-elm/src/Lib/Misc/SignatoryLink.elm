module Lib.Misc.SignatoryLink exposing (..)

import Lib.Types.SignatoryLink exposing (..)
import String exposing (trim)


findSignatoryField : (SignatoryField -> Maybe a) -> SignatoryLink -> Maybe a
findSignatoryField p (SignatoryLink siglink) =
    siglink.fields
        |> List.filterMap p
        |> List.head


getPersonalNumberField : SignatoryLink -> Maybe SignatoryField
getPersonalNumberField =
    findSignatoryField
        (\field ->
            case field of
                SignatoryPersonalNumberField _ ->
                    Just field

                _ ->
                    Nothing
        )


getTextField : (SignatoryField -> Maybe String) -> SignatoryLink -> String
getTextField p =
    findSignatoryField p >> Maybe.withDefault ""


getPersonalNumber : SignatoryLink -> String
getPersonalNumber =
    getTextField
        (\field ->
            case field of
                SignatoryPersonalNumberField { value } ->
                    Just value

                _ ->
                    Nothing
        )


getEmail : SignatoryLink -> String
getEmail =
    getTextField
        (\field ->
            case field of
                SignatoryEmailField { value } ->
                    Just value

                _ ->
                    Nothing
        )


getMobileNumber : SignatoryLink -> String
getMobileNumber =
    getTextField
        (\field ->
            case field of
                SignatoryMobileField { value } ->
                    Just value

                _ ->
                    Nothing
        )


getFirstName : SignatoryLink -> String
getFirstName =
    getTextField
        (\field ->
            case field of
                SignatoryNameField { value, nameOrder } ->
                    if nameOrder == 1 then
                        Just value

                    else
                        Nothing

                _ ->
                    Nothing
        )


getLastName : SignatoryLink -> String
getLastName =
    getTextField
        (\field ->
            case field of
                SignatoryNameField { value, nameOrder } ->
                    if nameOrder == 2 then
                        Just value

                    else
                        Nothing

                _ ->
                    Nothing
        )



-- 'Smart'... right.


getSmartName : SignatoryLink -> Maybe String
getSmartName sl =
    case trim (trim (getFirstName sl) ++ " " ++ trim (getLastName sl)) of
        "" ->
            Nothing

        name ->
            Just name
