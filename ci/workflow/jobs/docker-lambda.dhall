let BackendWorkflow = ../lib/backend-workflow.dhall

let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let GHCVersion = ../type/GHCVersion.dhall
let default-runner = ../config/default-runner.dhall

in
BackendWorkflow.createWorkflow
  BackendWorkflow.Args ::
  { name = "Backend Tests using local pdftools in Docker"
  , ghc-version = GHCVersion.Type.ghc88
  , nix-shell = "backend-shell"
  , runs-on = [ "ubuntu-latest" ]
  , use-pdftools-lambda = False
  , triggers = Workflow.Triggers ::
      { push = Some Workflow.BranchSpec ::
          { branches = Some [ "master", "staging", "production" ]
          , paths = Some [ "nix/source/pdftools.nix" ]
          }
      , pull_request = Some Workflow.BranchSpec ::
          { paths = Some [ "nix/source/pdftools.nix" ] }
      }
  }
