let Step = ./Step.dhall

let RunsOn = < self-hosted | linux | ubuntu-latest >

let Job =
  { runs-on: List RunsOn
  , steps: List Step.Type
  }

in
{ Job =
    { Type = Job
    , default = {=}
    }
, RunsOn = RunsOn
}
