let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let GHCVersion = ../type/GHCVersion.dhall
let setupSteps = ../lib/setup-steps.dhall
let default-runner = ../config/default-runner.dhall

let Json = ../type/Json.dhall

let build-kontrakcja-dist = Job.Job ::
    { runs-on = default-runner
    , steps =
      [ setupSteps.checkout-step
      , setupSteps.nix-step
      , setupSteps.cachix-step
      , Step ::
          { name = "Setup SSH (include frontend keys)"
          , env = Some (toMap
              { SSH_KEY_PDFTOOLS = "\${{ secrets.SSH_KEY_PDFTOOLS }}"
              , SSH_KEY_NEW_FRONTEND = "\${{ secrets.SSH_KEY_NEW_FRONTEND }}"
              , SSH_KEY_FLOW_FRONTEND = "\${{ secrets.SSH_KEY_FLOW_FRONTEND }}"
              })
          , run = Some "./ci/workflow/scripts/setup-ssh-frontend.sh"
          }
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
              nix-shell -A ghc88.dist-shell release.nix --run "./ci/workflow/scripts/run-dist-tests.sh"
              ''
          }
      ]
    }
in
Workflow.Workflow ::
  { name = "Build Nix Dist"
  , on = Some Workflow.Triggers ::
      { push = Some (Workflow.BranchSpec ::
          { branches = Some [ "master", "staging", "production", "nix" ]
          })
      -- Uncomment this line to temporary enable running this workflow in PR
      -- , pull_request = Some Workflow.BranchSpec.default
      }
  , jobs = toMap
      { build-kontrakcja-dist = build-kontrakcja-dist
      }
  }
