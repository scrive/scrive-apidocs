let BackendWorkflow = ../lib/backend-workflow.dhall

let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let NixShell = ../type/NixShell.dhall
let GHCVersion = ../type/GHCVersion.dhall

in
BackendWorkflow.createWorkflow
  BackendWorkflow.Args ::
  { name = "GHC 8.8 Backend Tests"
  , ghcVersion = GHCVersion.Type.ghc88
  , nixShell = NixShell.Type.manual-shell
  , runsOn =
      [ Job.RunsOn.ubuntu-latest
      ]
  , triggers = Workflow.Triggers ::
      { pull_request = Some Workflow.BranchSpec.default
      , push = Some
          { branches = Some [ "master" ]
          }
      }
  }
