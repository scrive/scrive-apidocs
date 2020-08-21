let BackendWorkflow = ../lib/backend-workflow.dhall

let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let NixShell = ../type/NixShell.dhall
let GHCVersion = ../type/GHCVersion.dhall

in
BackendWorkflow.createWorkflow
  BackendWorkflow.Args ::
  { name = "GHC 8.8 Backend Tests (Nix)"
  , ghcVersion = GHCVersion.Type.ghc88
  , nixShell = NixShell.Type.dev-shell-optimized
  , runsOn =
      [ Job.RunsOn.ubuntu-latest
      ]
  , triggers = Workflow.Triggers ::
      { push = Some Workflow.BranchSpec ::
          { branches = Some [ "master", "staging", "production" ]
          }
      , pull_request = Some Workflow.BranchSpec ::
          { paths = Some [ "**.hs", "**.cabal" ] }
      }
  }
