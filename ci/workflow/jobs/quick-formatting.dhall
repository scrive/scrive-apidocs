let FormattingWorkflow = ../lib/formatting-workflow.dhall

let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let GHCVersion = ../type/GHCVersion.dhall

in
FormattingWorkflow.createWorkflow
  FormattingWorkflow.Args ::
  { name = "Quick Formatting Tests"
  , runsOn =
      [ Job.RunsOn.ubuntu-latest
      ]
  , quickFormat = True
  , triggers = Workflow.Triggers ::
      { pull_request = Some Workflow.BranchSpec ::
          { paths = Some [ "**.hs" ] }
      }
  }
