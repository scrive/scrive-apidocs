let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let GHCVersion = ../type/GHCVersion.dhall
let default-runner = ../config/default-runner.dhall
let setupSteps = ../lib/setup-steps.dhall

let shell = "ghc88.frontend-shell"

let frontendTests = Job.Job ::
  -- Run frontend test on cloud runner for now
  -- until we have more runner capacity
  -- { runs-on = default-runner
  { runs-on = [ "ubuntu-latest" ]

  , steps =
      setupSteps.setup-steps #
      [ setupSteps.nix-shell-step shell
      , Step ::
        { name = "Test Frontend"
        , run = Some ''
            nix-shell -A ${shell} release.nix --run \
              ./ci/workflow/scripts/run-frontend-tests.sh
            ''
        }
      ]
  }
in
Workflow.Workflow ::
  { name = "Frontend Tests"
  , on = Some Workflow.Triggers ::
      { push = Some (Workflow.BranchSpec ::
          { branches = Some [ "master", "staging", "production" ]
          })
      , pull_request = Some Workflow.BranchSpec ::
          { paths = Some [ "frontend/**", "frontend-elm/**" ] }
      }
  , jobs = toMap
      { frontend-tests = frontendTests
      }
  }
