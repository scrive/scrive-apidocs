let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let GHCVersion = ../type/GHCVersion.dhall
let setupSteps = ../lib/setup-steps.dhall
let default-runner = ../config/default-runner.dhall

let Json = ../type/Json.dhall

let cache-nix-deps = Job.Job ::
    { runs-on =
        -- Run on self hosted MacOS, because cloud runner is wayyy too expensive
        -- Someone has to setup the self hosted runner on their Mac to run this
        [ "self-hosted"
        , "macos"
        ]
    , steps = setupSteps.setup-steps #
      [ Step ::
          { name = "Cache Nix Deps"
          , timeout-minutes = Some 360
          , env = Some (toMap
              { CACHIX_SIGNING_KEY = "\${{ secrets.CACHIX_SIGNING_KEY }}"
              , nix_collect_garbage = "true"
              })
          , run = Some ''
              ./ci/workflow/scripts/cache-nix-deps.sh ghc88.dev-shell
              ''
          }
      ]
    }
in
Workflow.Workflow ::
  { name = "Cache Nix Dependencies (MacOS)"
  , on = Some Workflow.Triggers ::
      { push = Some (Workflow.BranchSpec ::
          -- Only trigger this workflow by (force) pushing to the nix-mac branch.
          -- Mac cloud instance is 10x more expensive and we want to only run it when necessary.
          { branches = Some [ "nix-mac" ]
          , paths = Some [ "nix/**", "**.cabal" ]
          })
      -- , pull_request = None Workflow.BranchSpec.Type
      -- Uncomment this line to temporary enable running this workflow in PR
      -- , pull_request = Some Workflow.BranchSpec.default
      }
  , jobs = toMap
      { cache-nix-deps = cache-nix-deps
      }
  }
