let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let GHCVersion = ../type/GHCVersion.dhall
let NixShell = ../type/NixShell.dhall
let CreateJob = ../lib/create-job.dhall

let
  frontendTests =
    CreateJob.createJob
      { ghcVersion = GHCVersion.Type.ghc88
      , nixShell = NixShell.Type.dev-shell-optimized
      , nixExtraArgs = Some ''
          --arg extra-run-deps "pkgs: hsPkgs: [ pkgs.chromium ]"''

      , runsOn =
          [ Job.RunsOn.ubuntu-latest
          ]
      , steps =
          [ Step ::
            { name = "Test Frontend"
            , run = Some "./ci/scripts/run-frontend-tests.sh"
            }
          ]
      }
in
Workflow.Workflow ::
  { name = "Frontend Tests"
  , on = Some Workflow.Triggers ::
      { pull_request = Some Workflow.BranchSpec.default
      , push = Some
          { branches = Some [ "master", "staging", "production" ]
          }
      }
  , jobs = toMap
      { frontend-tests = frontendTests
      }
  }
