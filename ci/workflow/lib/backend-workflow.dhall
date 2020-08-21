let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let GHCVersion = ../type/GHCVersion.dhall
let NixShell = ../type/NixShell.dhall
let CreateJob = ./create-job.dhall
let Json = ../type/Json.dhall

let Args =
  { Type =
      { name: Text
      , ghcVersion: GHCVersion.Type
      , nixShell: NixShell.Type
      , cacheCabal: Bool
      , runsOn: List Job.RunsOn
      , triggers: Workflow.Triggers.Type
      , quickFormat: Bool
      }
  , default =
      { ghcVersion = GHCVersion.Type.ghc88
      , nixShell = NixShell.Type.dev-shell-optimized
      , cacheCabal = False
      , quickFormat = False
      }
  }

let createWorkflow =
  \(args: Args.Type) ->
    let inArgs =
      { ghcVersion = args.ghcVersion
      , nixShell = args.nixShell
      , runsOn = args.runsOn
      , nixExtraArgs = None Text
      }

    let cacheCabalSteps =
      if args.cacheCabal
      then \(name: Text) ->
          [ Step ::
            { name = "Cache ~/.cabal"
            , uses = Some "actions/cache@v2"
            , with = Some toMap
              { path = Json.Str "~/.cabal"
              , key = Json.Str "\${{ runner.os }}-cabal-${name}-\${{ hashFiles('**/*.cabal') }}"
              , restore-keys = Json.Str ''
                ''${{ runner.os }}-cabal-${name}-''${{ hashFiles('**/*.cabal') }}
                ''${{ runner.os }}-cabal-${name}-
                ''
              }
            }
          ]
      else \(name: Text) -> ([] : List Step.Type)

    let createJob = \(name: Text) -> \(steps: List Step.Type) ->
      CreateJob.createJob
        ( inArgs // { steps = cacheCabalSteps name # steps } )

    let backendTests = CreateJob.createJob
      ( inArgs //
          { steps = cacheCabalSteps "backend-tests" #
            [ Step ::
              { name = "Build Kontrakcja"
              , timeout-minutes = Some 180
              , run = Some "cabal update; cabal configure --enable-tests --disable-optimization; cabal build all"
              }
            , Step ::
              { name = "Run Backend Tests"
              , timeout-minutes = Some 180
              , run = Some "./ci/scripts/run-backend-tests.sh"
              , env = Some toMap
                  { PDFTOOLS_CONFIG = "\${{ secrets.PDFTOOLS_CONFIG }}"
                  }
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
      )

    let quickVal = if args.quickFormat then "-quick" else ""

    let formatting = CreateJob.createJob
      ( inArgs //
          { steps = cacheCabalSteps "test-formatting" #
            [ Step ::
              { name = "Test HLint"
              , if = Some "\${{ always() }}"
              , env = Some (toMap
                  { quick = quickVal
                  })
              , run = Some "./ci/scripts/test-hlint.sh"
              }
            , Step ::
              { name = "Test Formatting"
              , env = Some (toMap
                  { quick = quickVal
                  })
              , run = Some "./ci/scripts/test-formatting.sh"
              }
            , Step ::
              { name = "Detect Old Templates"
              , if = Some "\${{ always() }}"
              , run = Some "./shake.sh detect-old-templates"
              }
            , Step ::
              { name = "Detect Old Localizations"
              , if = Some "\${{ always() }}"
              , run = Some "./shake.sh detect-old-localizations"
              }
            , Step ::
              { name = "Upload Formatting Patch"
              , uses = Some "actions/upload-artifact@v2"
              , if = Some "\${{ failure() }}"
              , with = Some toMap
                { name = Json.Str "formatting-patch"
                , path = Json.Str "_build/formatting.patch"
                }
              }
            ]
            , nixExtraArgs = Some "--arg extra-run-deps 'pkgs: hsPkgs: [ pkgs.git ]'"
          }
      )

    let jobs =
      { formatting = formatting
      , backend-tests = backendTests
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
