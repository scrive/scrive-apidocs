let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let GHCVersion = ../type/GHCVersion.dhall
let default-runner = ../config/default-runner.dhall
let setupSteps = ../lib/setup-steps.dhall
let Json = ../type/Json.dhall

let buildApiDocs = Job.Job ::
  { runs-on = default-runner
  , steps =
      setupSteps.setup-steps #
      [ Step ::
        { name = "Build API Docs"
        , run = Some ''
            nix-build -A api-docs release.nix
            mkdir -p _build

            cp -r result/* _build/

            find _build -type d -exec chmod 755 {} \;
            find _build -type f -exec chmod 644 {} \;
            ''
        }
      , Step ::
        { name = "Publish API Docs"
        , uses = Some "peaceiris/actions-gh-pages@v3"
        , if = Some "\${{ github.ref == 'refs/heads/production' }}"
        , with = Some (toMap
            { deploy_key = Json.Str "\${{ secrets.SSH_KEY_API_DOCS }}"
            , publish_dir = Json.Str "./_build"
            , external_repository = Json.Str "scrive/scrive-apidocs"
            })
        }
      ]
  }
in
Workflow.Workflow ::
  { name = "API Docs"
  , on = Some Workflow.Triggers ::
      { push = Some (Workflow.BranchSpec ::
          { branches = Some [ "master", "staging", "production", "nix" ]
          , paths = Some [ "api-docs/**" ]
          })
      , pull_request = Some Workflow.BranchSpec ::
          { paths = Some [ "api-docs/**" ]
          }
      }
  , jobs = toMap
      { api-docs = buildApiDocs
      }
  }
