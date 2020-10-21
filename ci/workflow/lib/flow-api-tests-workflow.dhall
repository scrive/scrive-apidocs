let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let Json = ../type/Json.dhall

let Args =
  {
  , run-flags: Text
  }

in
\(args: Args) ->
  let createTest = Job.Job ::
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
              { name = "Run Flow API Tests"
              , env = Some (toMap
                  { GITCRYPT_KEY = "\${{ secrets.GITCRYPT_KEY }}"
                  })
              , run = Some ''
                  sudo apt-get update && sudo apt-get install -y git-crypt python3 virtualenv
                  cd backend/flow/test
                  echo "$GITCRYPT_KEY" | base64 -d > git-crypt.key
                  git-crypt unlock git-crypt.key
                  ./run_tests.sh ${args.run-flags}
                  ''
              }
            ]
        }
  in
  Workflow.Workflow ::
    { name = "Flow API Tests (script changes)"
    , on = Some Workflow.Triggers ::
        { push = Some Workflow.BranchSpec ::
          { branches = Some [ "master" ] }
        , pull_request = Some Workflow.BranchSpec ::
          { paths = Some [ "backend/flow/test/**" ] }
        }
    , jobs = toMap
        { dev = createTest
        }
    }
