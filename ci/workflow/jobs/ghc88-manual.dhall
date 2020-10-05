let BackendWorkflow = ../lib/backend-workflow.dhall

let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let GHCVersion = ../type/GHCVersion.dhall
let default-runner = ../config/default-runner.dhall

in
BackendWorkflow.createWorkflow
  BackendWorkflow.Args ::
  { name = "GHC 8.8 Backend Tests (Manual)"
  , ghc-version = GHCVersion.Type.ghc88
  , nix-shell = "manual-backend-shell"
  , cache-cabal = True
  , runs-on = default-runner
  , triggers = Workflow.Triggers ::
      { push = Some (Workflow.BranchSpec ::
          { branches = Some [ "master", "staging", "production" ]
          })
      -- Uncomment this line to temporary enable running this workflow in PR
      -- , pull_request = Some Workflow.BranchSpec ::
      --     { paths = Some [ "**.hs", "**.cabal", "cabal.project.freeze" ] }
      }
  }
