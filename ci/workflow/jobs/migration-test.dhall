let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Json = ../type/Json.dhall
let Workflow = ../type/Workflow.dhall
let GHCVersion = ../type/GHCVersion.dhall
let setupSteps = ../lib/setup-steps.dhall
let default-runner = ../config/default-runner.dhall

let shell = "ghc88.backend-shell"

let migration-tests =
    Job.Job ::
      { runs-on = default-runner
      , steps =
          [ Step ::
              { name = "Checkout Code"
              , uses = Some "actions/checkout@v2"
              , with = Some (toMap
                  { fetch-depth = Json.Nat 0
                  })
              }
          ] # setupSteps.install-steps #
          [ setupSteps.nix-shell-step shell
          , Step ::
              { name = "Load Nix Shell"
              , run = Some ''
                  nix-shell -j4 -A ${shell} release.nix \
                    --run "echo Loaded Nix shell"
                  ''
              }
          , Step ::
            { name = "Run Migration Tests"
            , env = Some (toMap
                { BASE_BRANCH = "master"
                , PDFTOOLS_CONFIG = "\${{ secrets.PDFTOOLS_CONFIG }}"
                })
            , run = Some ''
                nix-shell -j4 -A ${shell} release.nix \
                  --run "./ci/workflow/scripts/run-migration-tests.sh"
                ''
            }
          ]
      }
in
Workflow.Workflow ::
  { name = "Migration Tests"
  , on = Some Workflow.Triggers ::
      { pull_request = Some (Workflow.BranchSpec ::
          { paths = Some
              [ "backend/migrate/**"
              ]
          })
      }
  , jobs = toMap
      { migration-tests = migration-tests
      }
  }
