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

  -- Run quick formatting test on cloud runner for now
  -- until we have more runner capacity
  -- , runs-on = default-runner
  , runs-on = [ "ubuntu-latest" ]

  , quickFormat = True
  , triggers = Workflow.Triggers ::
      { pull_request = Some Workflow.BranchSpec ::
          { paths = Some [ "**.hs" ] }
      }
  }
