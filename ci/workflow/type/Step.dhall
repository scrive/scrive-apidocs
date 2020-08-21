let Map = https://prelude.dhall-lang.org/Map/Type

let Json = ./Json.dhall

let Step =
  { name: Text
  , uses: Optional Text
  , run: Optional Text
  , if: Optional Text
  , env: Optional (Map Text Text)
  , with: Optional (Map Text Json)
  , timeout-minutes: Optional Natural
  }

let default =
  { uses = None Text
  , run = None Text
  , if = None Text
  , env = None (Map Text Text)
  , with = None (Map Text Json)
  , timeout-minutes = None Natural
  }

in
{ Type = Step
, default = default
}
