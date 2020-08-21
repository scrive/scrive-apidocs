let BackendWorkflow = ../lib/backend-workflow.dhall

let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let NixShell = ../type/NixShell.dhall
let GHCVersion = ../type/GHCVersion.dhall

in
BackendWorkflow.createWorkflow
  BackendWorkflow.Args ::
  { name = "GHC 8.8 Backend Tests (Nix) - Pull Requests"
  , ghcVersion = GHCVersion.Type.ghc88
  , nixShell = NixShell.Type.dev-shell-optimized
  , quickFormat = True
  , runsOn =
      [ Job.RunsOn.ubuntu-latest
      ]
  , triggers = Workflow.Triggers ::
      { pull_request = Some Workflow.BranchSpec ::
          { paths = Some [ "**.hs", "**.cabal" ] }
      }
  }
