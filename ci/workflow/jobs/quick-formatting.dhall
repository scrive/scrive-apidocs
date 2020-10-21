let FormattingWorkflow = ../lib/formatting-workflow.dhall

let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let GHCVersion = ../type/GHCVersion.dhall
let default-runner = ../config/default-runner.dhall

in
FormattingWorkflow.createWorkflow
  FormattingWorkflow.Args ::
  { name = "Quick Formatting Tests"
  , runs-on = default-runner
  , quickFormat = True
  , triggers = Workflow.Triggers ::
      { push = Some (Workflow.BranchSpec ::
          { branches = Some [ "nix" ]
          })
      , pull_request = Some Workflow.BranchSpec ::
          { paths = Some [ "**.hs" ] }
      }
  }
