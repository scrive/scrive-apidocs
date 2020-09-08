let FormattingWorkflow = ../lib/formatting-workflow.dhall

let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let GHCVersion = ../type/GHCVersion.dhall

in
FormattingWorkflow.createWorkflow
  FormattingWorkflow.Args ::
  { name = "Formatting Tests"
  , runsOn =
      [ Job.RunsOn.ubuntu-latest
      ]
  , quickFormat = False
  , triggers = Workflow.Triggers ::
      { push = Some Workflow.BranchSpec ::
          { branches = Some [ "master", "staging", "production" ]
          }
      }
  }
