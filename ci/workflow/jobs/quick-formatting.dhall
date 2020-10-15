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
  , runs-on = [ "ubuntu-20.04" ]
  , quickFormat = True
  , triggers = Workflow.Triggers ::
      { pull_request = Some Workflow.BranchSpec ::
          { paths = Some [ "**.hs" ] }
      }
  }
