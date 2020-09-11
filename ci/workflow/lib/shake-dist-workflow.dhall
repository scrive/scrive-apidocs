let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let GHCVersion = ../type/GHCVersion.dhall
let default-runner = ../config/default-runner.dhall
let setupSteps = ./setup-steps.dhall

let Json = ../type/Json.dhall

let Args =
  { name: Text
  , branches: List Text
  , nginx-rules-path: Text
  , nginx-rules-path-alternative: Text
  , nginx-default-rule: Text
  }

in
\(args: Args) ->
  let shell = "ghc88.backend-shell"

  let run-shake-dist = Job.Job ::
    { runs-on = default-runner
    , steps =
        setupSteps.setup-steps #
        [ setupSteps.nix-shell-step shell
        , Step ::
            { name = "Run Shake Dist"
            , env = Some (toMap
                { TEAMCITY_VERSION = "1"
                , BUILD_NUMBER = "github-\${{ github.run_id }}"
                , BUILD_VCS_NUMBER = "\${{ github.sha }}"
                , NGINX_CONF_RULES_PATH = args.nginx-rules-path
                , NGINX_CONF_RULES_PATH_ALTERNATIVE = args.nginx-rules-path
                , NGINX_CONF_DEFAULT_RULE = args.nginx-default-rule
                })
            , run = Some ''
                nix-shell -A ${shell} release.nix --run \
                  ./ci/workflow/scripts/run-shake-dist.sh
                ''
            }
        , Step ::
          { name = "Upload Shake Dist"
          , uses = Some "actions/upload-artifact@v2"
          , with = Some (toMap
            { name = Json.Str "kontrakcja-shake-dist"
            , path = Json.Str "_build/kontrakcja.tar.gz"
            })
          }
        ]
    }
  in
  Workflow.Workflow ::
    { name = "Build Shake Dist (${args.name})"
    , on = Some Workflow.Triggers ::
        { push = Some (Workflow.BranchSpec ::
            { branches = Some args.branches
            })
        -- , pull_request = Some Workflow.BranchSpec.default
        }
    , jobs = toMap
        { run-shake-dist = run-shake-dist
        }
    }
