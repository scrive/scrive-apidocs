module EnumExtra exposing (Enum, allValues, findEnumValue, makeEnum, toHumanString, toString)

import Enum as EnumOrig


type alias Enum a =
    { enum : EnumOrig.Enum a
    , toHumanString : a -> String
    , allValues : List a
    }


makeEnum : List a -> (a -> String) -> (a -> String) -> Enum a
makeEnum allVals toStr toHumanStr =
    { enum = EnumOrig.makeEnum allVals toStr
    , toHumanString = toHumanStr
    , allValues = allVals
    }


allValues : Enum a -> List a
allValues enum =
    enum.allValues


toString : Enum a -> a -> String
toString enum value =
    EnumOrig.toString enum.enum value


toHumanString : Enum a -> a -> String
toHumanString enum value =
    enum.toHumanString value


findEnumValue : Enum a -> String -> Result String a
findEnumValue enum str =
    EnumOrig.findEnumValue enum.enum str
