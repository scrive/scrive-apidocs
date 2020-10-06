let listMap = https://prelude.dhall-lang.org/List/map
let concat = https://prelude.dhall-lang.org/Text/concat

let config = ../config.dhall
let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let GHCVersion = ../type/GHCVersion.dhall
let setupSteps = ../lib/setup-steps.dhall
let default-runner = ../config/default-runner.dhall

let Json = ../type/Json.dhall

let cache-nix-deps = Job.Job ::
    { runs-on = default-runner
    , strategy = Some
        { matrix = toMap
            { nix-shell =
                [ "ghc88.lint-shell"
                , "ghc88.dist-shell"
                , "ghc88.backend-shell"
                , "ghc88.frontend-shell"
                , "ghc88.selenium-shell"
                , "ghc88.detect-unused-shell"
                , "ghc88.manual-backend-shell"
                , "ghc88.kontrakcja-frontend"
                , "ghc86.backend-shell"
                , "ghc86.manual-backend-shell"
                ]
            }
        }
    , steps = setupSteps.setup-steps #
      [ Step ::
          { name = "Cache Nix Deps"
          , timeout-minutes = Some 360
          , env = Some (toMap
              { CACHIX_SIGNING_KEY = "\${{ secrets.CACHIX_SIGNING_KEY }}"
              , nix_collect_garbage =
                  if config.nix-collect-garbage
                  then "nix-collect-garbage"
                  else "true"
              })
          , run = Some ''
              ./ci/workflow/scripts/cache-nix-deps.sh ''${{ matrix.nix-shell }}
              ''
          }
      ]
    }

let cache-new-frontend-deps = Job.Job ::
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
          { name = "Cache Nix Frontend Deps"
          , timeout-minutes = Some 360
          , env = Some (toMap
              { CACHIX_SIGNING_KEY = "\${{ secrets.CACHIX_SIGNING_KEY }}"
              , nix_collect_garbage =
                  if config.nix-collect-garbage
                  then "nix-collect-garbage"
                  else "true"
              })
          , run = Some ''
              ./ci/workflow/scripts/cache-nix-deps.sh new-frontend
              ''
          }
      ]
  }
in
Workflow.Workflow ::
  { name = "Cache Nix Dependencies"
  , on = Some Workflow.Triggers ::
      { push = Some Workflow.BranchSpec ::
          { branches = Some [ "master", "nix" ]
          , paths = Some [ "nix/**", "**.cabal" ]
          }
        , pull_request = Some Workflow.BranchSpec ::
          { branches = Some [ "master", "nix" ]
          , paths = Some [ "nix/**", "**.cabal" ]
          }
      }
  , jobs = toMap
      { cache-nix-deps = cache-nix-deps
      , cache-new-frontend-deps = cache-new-frontend-deps
      }
  }
