let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let Workflow = ../type/Workflow.dhall
let GHCVersion = ../type/GHCVersion.dhall
let setupSteps = ./setup-steps.dhall

let Json = ../type/Json.dhall

let Args =
  { Type =
      { name: Text
      , quickFormat: Bool
      , runs-on: List Text
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
      , nixShell = "dev-shell"
      , runs-on = args.runs-on
      }

    let quickVal = if args.quickFormat then "-quick" else ""

    let checkoutStep = if args.quickFormat
      then Step ::
        { name = "Checkout Code"
        , with = Some (toMap
            { fetch-depth = Json.Nat 0
            })
        , uses = Some "actions/checkout@v2"
        }
      else Step ::
        { name = "Checkout Code"
        , uses = Some "actions/checkout@v2"
        }

    let createJob = \(steps: List Step.Type) ->
      Job.Job ::
        { runs-on = args.runs-on
        , steps =
            [ checkoutStep ]
            # setupSteps.install-steps
            # steps
        }

    let formatting = createJob
      [ setupSteps.nix-shell-step "ghc88.lint-shell"
      , Step ::
        { name = "Test Formatting"
        , env = Some (toMap
            { quick = quickVal
            })
        , run = Some ''
            nix-shell -A ghc88.lint-shell release.nix \
              --run "./ci/workflow/scripts/test-formatting.sh"
            ''
        }
      , Step ::
        { name = "Upload Formatting Patch"
        , uses = Some "actions/upload-artifact@v2"
        , if = Some "\${{ failure() }}"
        , with = Some (toMap
          { name = Json.Str "formatting.patch"
          , path = Json.Str "_build/formatting.patch"
          })
        }
      ]

    let hlint = createJob
      [ setupSteps.nix-shell-step "ghc88.lint-shell"
      , Step ::
        { name = "Test HLint"
        , env = Some (toMap
            { quick = quickVal
            })
        , run = Some ''
            nix-shell -A ghc88.lint-shell release.nix \
              --run "./ci/workflow/scripts/test-hlint.sh"
            ''
        }
      , Step ::
        { name = "Upload HLint Patch"
        , uses = Some "actions/upload-artifact@v2"
        , if = Some "\${{ failure() }}"
        , with = Some (toMap
          { name = Json.Str "hlint.patch"
          , path = Json.Str "_build/hlint.patch"
          })
        }
      ]

    let detect-unused = createJob
      [ setupSteps.nix-shell-step "ghc88.detect-unused-shell"
      , Step ::
        { name = "Detect Old Templates"
        , run = Some ''
            nix-shell -A ghc88.detect-unused-shell release.nix \
              --run "detect_old_templates"
            ''
        }
      , Step ::
        { name = "Detect Old Localizations"
        , run = Some ''
            nix-shell -A ghc88.detect-unused-shell release.nix \
              --run "detect_old_localizations"
            ''
        }
      ]

    let jobs =
      { formatting = formatting
      , hlint = hlint
      , detect-unused = detect-unused
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
