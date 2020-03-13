module EnumExtra exposing (..)

import Enum as EnumOrig
import Dict as DictOrig
import Maybe.Extra as Maybe


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

fromString : Enum a -> String -> Maybe a
fromString enum = Result.toMaybe << findEnumValue enum

type alias Dict k v =
    { dict : DictOrig.Dict String v
    , enum : Enum k
    }

empty : Enum k -> Dict k v
empty enum = { dict = DictOrig.empty, enum = enum }

insert : k -> v -> Dict k v -> Dict k v
insert k v d = { d | dict = DictOrig.insert (toString d.enum k) v d.dict }

remove : k -> Dict k v -> Dict k v
remove k d = { d | dict = DictOrig.remove (toString d.enum k) d.dict }

get : k -> Dict k v -> Maybe v
get k d = DictOrig.get (toString d.enum k) d.dict

fromList : Enum k -> List (k,v) -> Dict k v
fromList enum kvs =
  { dict = DictOrig.fromList <| List.map (\(k,v) -> (toString enum k, v)) kvs
  , enum = enum
  }

toList : Dict k v -> List (k,v)
toList d = Maybe.values
  <| List.map (\(s,v) -> fromString d.enum s |> Maybe.andThen (\k -> Just (k,v)))
  <| DictOrig.toList d.dict

values : Dict k v -> List v
values = DictOrig.values << .dict

member : k -> Dict k v -> Bool
member k v = Maybe.isJust <| get k v
