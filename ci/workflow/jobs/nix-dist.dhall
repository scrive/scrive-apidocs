let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let GHCVersion = ../type/GHCVersion.dhall
let setupSteps = ../lib/setup-steps.dhall
let default-runner = ../config/default-runner.dhall

let Json = ../type/Json.dhall

let build-kontrakcja-dist = Job.Job ::
    { runs-on = default-runner
    , steps = setupSteps.setup-steps #
      [ setupSteps.nix-gc-step
      , Step ::
          { name = "Build Kontrakcja Dist"
          , run = Some ''
              nix-build -o result -A ghc88.dist release.nix
              ''
          }
      , Step ::
          { name = "Run Dist Tests"
          , env = Some (toMap
              { PDFTOOLS_CONFIG = "\${{ secrets.PDFTOOLS_CONFIG }}"
              })
          , run = Some ''
              nix-shell -j4 -A ghc88.dist-shell release.nix --run "./ci/workflow/scripts/run-dist-tests.sh"
              ''
          }
      ]
    }
in
Workflow.Workflow ::
  { name = "Build Nix Dist"
  , on = Some Workflow.Triggers ::
      { push = Some (Workflow.BranchSpec ::
          -- Disabling this workflow on main branches because Nix dist is broken.
          -- Ses [FLOW-366].
          -- { branches = Some [ "master", "staging", "production" ]
          { branches = Some [ "nix" ]
          })
      -- Uncomment this line to temporary enable running this workflow in PR
      -- , pull_request = Some Workflow.BranchSpec.default
      }
  , jobs = toMap
      { build-kontrakcja-dist = build-kontrakcja-dist
      }
  }
