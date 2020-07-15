let Job = ./Job.dhall
let Map = https://prelude.dhall-lang.org/Map/Type

let BranchSpec =
  { Type =
      { branches: Optional (List Text) }
  , default =
      { branches = None (List Text) }
  }

let Triggers =
  { Type =
      { push: Optional BranchSpec.Type
      , pull_request: Optional BranchSpec.Type
      }
  , default =
      { push = None BranchSpec.Type
      , pull_request = None BranchSpec.Type
      }
  }

let Workflow =
  { name: Text
  , on: Optional Triggers.Type
  , jobs: Map Text Job.Job.Type
  }

in
{ Workflow =
    { Type = Workflow
    , default = {=}
    }
, Triggers = Triggers
, BranchSpec = BranchSpec
}
