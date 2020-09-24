let Job = ./Job.dhall
let config = ../config.dhall
let Map = https://prelude.dhall-lang.org/Map/Type

let BranchSpec =
  { Type =
      { branches: Optional (List Text)
      , paths: Optional (List Text)
      }
  , default =
      { branches = None (List Text)
      , paths = None (List Text)
      }
  }

let CronField = { cron: Text }

let Triggers =
  { Type =
      { push: Optional BranchSpec.Type
      , pull_request: Optional BranchSpec.Type
      , schedule: Optional (List CronField)
      }
  , default =
      { push = None BranchSpec.Type
      , schedule = None (List CronField)
      , pull_request =
          if config.trigger-all-jobs-for-prs
          then Some BranchSpec.default
          else None BranchSpec.Type
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
