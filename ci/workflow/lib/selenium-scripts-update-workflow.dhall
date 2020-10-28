let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let Json = ../type/Json.dhall

let Args =
  {
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
        { runs-on = ["ubuntu-latest"]
        , steps =
            [
            , Step ::
              { name = "Cancel previous runs"
              , uses = Some "styfle/cancel-workflow-action@0.4.1"
              , with = Some (toMap
                { access_token = Json.Str "\${{ secrets.GITHUB_TOKEN }}"
                })
              }
            , Step ::
              { name = "Checkout Code"
              , uses = Some "actions/checkout@v2"
              }
            , Step ::
              { name = "Run Selenium Tests (${args2.name})"
              , env = Some (toMap
                  { SELENIUM_CONFIG = "\${{ secrets.${args.config-key} }}"
                  })
              , run = Some ''
                  sudo apt-get update && sudo apt-get install -y poppler-utils python3 virtualenv
                  virtualenv --python=$(which python3) venv
                  source ./venv/bin/activate
                  python --version && pip --version
                  cd ${args2.test-dir}
                  echo "$SELENIUM_CONFIG" > ../config.py
                  pip install -r ../requirements.txt
                  python run.py ${args.run-flags}
                  ''
              }
            , Step ::
              { name = "Upload Screenshots"
              , uses = Some "actions/upload-artifact@v2"
              , if = Some "\${{ always() }}"
              , with = Some (toMap
                { name = Json.Str "screenshots"
                , path = Json.Str "${args2.test-dir}/screenshots"
                })
              }
            , Step ::
              { name = "Upload Artifacts"
              , uses = Some "actions/upload-artifact@v2"
              , if = Some "\${{ always() }}"
              , with = Some (toMap
                { name = Json.Str "artifacts"
                , path = Json.Str "${args2.test-dir}/artifacts"
                })
              }
            ]
        }
  in
  Workflow.Workflow ::
    { name = "Selenium Tests (script changes)"
    , on = Some Workflow.Triggers ::
        {
        , pull_request = Some Workflow.BranchSpec ::
          { paths = Some [ "selenium-test/**" ] }
        }
    , jobs = toMap
        { general = createTest
            { name = "general"
            , test-dir = "selenium-test/general"
            }
        , language = createTest
            { name = "languages"
            , test-dir = "selenium-test/language-test"
            }
        , evidence-package = createTest
            { name = "evidence Package"
            , test-dir = "selenium-test/evidence_package"
            }
        }
    }
