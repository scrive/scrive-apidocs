let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let GHCVersion = ../type/GHCVersion.dhall
let default-runner = ../config/default-runner.dhall
let setupSteps = ./setup-steps.dhall

let Json = ../type/Json.dhall

let Args =
  { branch: Text
  , run-flags: Text
  , config-key: Text
  }

let InArgs =
  { name: Text
  , test-dir: Text
  }

in
\(args: Args) ->
  let createTest = \(args2: InArgs) ->
      Job.Job ::
        { runs-on = default-runner
        , steps =
            [ Step ::
                { name = "Checkout Code"
                , with = Some (toMap
                    { ref = Json.Str args.branch
                    })
                , uses = Some "actions/checkout@v2"
                }
            ] # setupSteps.install-steps #
            [ setupSteps.nix-shell-step "ghc88.selenium-shell"
            , Step ::
              { name = "Run Selenium Tests (${args2.name})"
              , env = Some (toMap
                  { SELENIUM_CONFIG = "\${{ secrets.${args.config-key} }}"
                  })
              , run = Some ''
                  nix-shell -A ghc88.selenium-shell release.nix --run \
                    "./ci/workflow/scripts/run-selenium-tests.sh ${args2.test-dir} ${args.run-flags}"
                  ''
              }
            , Step ::
              { name = "Upload Selenium Server log"
              , uses = Some "actions/upload-artifact@v2"
              , if = Some "\${{ always() }}"
              , with = Some (toMap
                { name = Json.Str "selenium-logs"
                , path = Json.Str "supervisor/logs"
                })
              }
            , Step ::
              { name = "Upload Screenshots"
              , uses = Some "actions/upload-artifact@v2"
              , if = Some "\${{ always() }}"
              , with = Some (toMap
                { name = Json.Str "screenshots"
                , path = Json.Str "selenium-test/${args2.test-dir}/screenshots"
                })
              }
            , Step ::
              { name = "Upload Artifacts"
              , uses = Some "actions/upload-artifact@v2"
              , if = Some "\${{ always() }}"
              , with = Some (toMap
                { name = Json.Str "artifacts"
                , path = Json.Str "selenium-test/${args2.test-dir}/artifacts"
                })
              }
            ]
        }
  in
  Workflow.Workflow ::
    { name = "Selenium Tests (${args.branch})"
    , on = Some Workflow.Triggers ::
        { schedule = Some
            [ { cron = "0 0 * * *" } ]
        --   Uncomment this line to temporary enable running this workflow in PR
        -- , pull_request = Some Workflow.BranchSpec.default
        }
    , jobs = toMap
        { general = createTest
            { name = "General"
            , test-dir = "general"
            }
        , language = createTest
            { name = "Languages"
            , test-dir = "language-test"
            }
        , evidence-package = createTest
            { name = "Evidence Package"
            , test-dir = "evidence_package"
            }
        }
    }
