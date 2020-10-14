let BackendWorkflow = ../lib/backend-workflow.dhall

let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let GHCVersion = ../type/GHCVersion.dhall
let default-runner = ../config/default-runner.dhall

in
BackendWorkflow.createWorkflow
  BackendWorkflow.Args ::
  { name = "GHC 8.8 Backend Tests (Nix)"
  , ghc-version = GHCVersion.Type.ghc88
  , nix-shell = "backend-shell"

  , runs-on = default-runner

  , triggers = Workflow.Triggers ::
      { push = Some Workflow.BranchSpec ::
          { branches = Some [ "master", "staging", "production" ]
          }
      , pull_request = Some Workflow.BranchSpec ::
          { paths = Some
              [ "**.hs"
              , "**.cabal"
              , "cabal.project.freeze"
              , "texts/**"
              , "nix/**"
              ]
          }
      }
  }
