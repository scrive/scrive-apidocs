let Map = https://prelude.dhall-lang.org/Map/Type

let Step = ./Step.dhall

let Strategy =
  { matrix: Map Text (List Text)
  }

let Job =
  { runs-on: List Text
  , steps: List Step.Type
  , needs: Optional Text
  , strategy: Optional Strategy
  }

in
{ Job =
    { Type = Job
    , default =
        { needs = None Text
        , strategy = None Strategy
        }
    }
, Strategy = Strategy
}
