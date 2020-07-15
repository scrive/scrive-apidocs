let Map = https://prelude.dhall-lang.org/Map/Type

let Step =
  { name: Text
  , uses: Optional Text
  , run: Optional Text
  , env: Optional (Map Text Text)
  , with: Optional (Map Text Text)
  , timeout-minutes: Optional Natural
  }

let default =
  { uses = None Text
  , run = None Text
  , env = None (Map Text Text)
  , with = None (Map Text Text)
  , timeout-minutes = None Natural
  }

in
{ Type = Step
, default = default
}
