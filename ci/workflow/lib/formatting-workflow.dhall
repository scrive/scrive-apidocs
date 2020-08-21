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
      , quickFormat: Bool
      , runsOn: List Job.RunsOn
      , triggers: Workflow.Triggers.Type
      }
  , default =
      { quickFormat = False
      }
  }

let createWorkflow =
  \(args: Args.Type) ->
    let inArgs =
      { ghcVersion = GHCVersion.Type.ghc88
      , nixShell = NixShell.Type.dev-shell-optimized
      , runsOn = args.runsOn
      , nixExtraArgs = None Text
      }

    let quickVal = if args.quickFormat then "-quick" else ""

    let formatting = CreateJob.createJob
      ( inArgs //
          { steps =
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
