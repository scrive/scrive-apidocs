let Map = https://prelude.dhall-lang.org/Map/Type

let config = ../config.dhall
let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let GHCVersion = ../type/GHCVersion.dhall
let Json = ../type/Json.dhall
let setupSteps = ./setup-steps.dhall

let Args =
  { Type =
      { name: Text
      , ghc-version: GHCVersion.Type
      , nix-shell: Text
      , cache-cabal: Bool
      , runs-on: List Text
      , triggers: Workflow.Triggers.Type
      , use-pdftools-lambda: Bool
      }
  , default =
      { ghc-version = GHCVersion.Type.ghc88
      , nix-shell = "dev-shell"
      , cache-cabal = False
      , use-pdftools-lambda = True
      }
  }

let createWorkflow =
  \(args: Args.Type) ->
    let shell = "${GHCVersion.format args.ghc-version}.${args.nix-shell}"

    -- We want to cache ~/.cabal using GitHub Actions cache
    -- when running on cloud runner to speed up manual shell
    -- builds of cabal dependencies
    let cache-cabalSteps =
      if args.cache-cabal
      then
        -- We cache ~/.cabal depending on the hash value of cabal.project.freeze
        -- The suffix like -1 are used to purge the cache on GitHub Actions if needed
        let hasher = "\${{ hashFiles('cabal.project.freeze') }}-2" in
        [ Step ::
          { name = "Cache ~/.cabal"
          , uses = Some "actions/cache@v2"
          , with = Some (toMap
              { path = Json.Str "~/.cabal"
              , key = Json.Str "\${{ runner.os }}-cabal-${hasher}"
              , restore-keys = Json.Str ''
                  ''${{ runner.os }}-cabal-${hasher}
                  ''
              })
          }
        ]
      else
        [] : List Step.Type

    let backendEnv = if args.use-pdftools-lambda
      then Some (toMap
            { PDFTOOLS_CONFIG = "\${{ secrets.PDFTOOLS_CONFIG }}"
            })
      else None (Map Text Text)

    let backendTests = Job.Job ::
      { runs-on = args.runs-on
      , steps =
          setupSteps.setup-steps #
          [ setupSteps.nix-shell-step shell ]
          # cache-cabalSteps #
          [ Step ::
            { name = "Build Kontrakcja"
            , timeout-minutes = Some 180
            , run = Some ''
                nix-shell -A ${shell} release.nix --run \
                  "cabal update; cabal configure --enable-tests --disable-optimization; cabal build all"
                ''
            }
          , Step ::
            { name = "Run Backend Tests"
            , timeout-minutes = Some 180
            , env = backendEnv
            , run = Some ''
                nix-shell -A ${shell} release.nix --run \
                  "./ci/workflow/scripts/run-backend-tests.sh"
                ''
            }
          , Step ::
            { name = "Build Haddock"
            , run = Some ''
                nix-shell -A ${shell} release.nix --run \
                  "cabal haddock all"
                ''
            }
          , Step ::
            { name = "Upload Logs"
            , uses = Some "actions/upload-artifact@v2"
            , if = Some "\${{ always() }}"
            , with = Some (toMap
              { name = Json.Str "logs"
              , path = Json.Str "logs"
              })
            }
          ]
      }

    let jobs =
      { backend-tests = backendTests
      }

    in
    Workflow.Workflow ::
      { name = args.name
      , on = Some args.triggers
      , jobs = toMap jobs
      }
in
{ Args = Args
, createWorkflow = createWorkflow
}
