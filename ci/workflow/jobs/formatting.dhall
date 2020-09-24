let FormattingWorkflow = ../lib/formatting-workflow.dhall

let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let GHCVersion = ../type/GHCVersion.dhall
let default-runner = ../config/default-runner.dhall

in
FormattingWorkflow.createWorkflow
  FormattingWorkflow.Args ::
  { name = "Formatting Tests"
  , runs-on = default-runner
  , quickFormat = False
  , triggers = Workflow.Triggers ::
      { push = Some (Workflow.BranchSpec ::
          { branches = Some [ "master", "staging", "production" ]
          })
      -- Uncomment this line to temporary enable running this workflow in PR
      -- , pull_request = Some Workflow.BranchSpec.default
      }
  }
