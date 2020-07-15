let listMap = https://prelude.dhall-lang.org/List/map
let optionalMap = https://prelude.dhall-lang.org/Optional/map

let Step = ../type/Step.dhall
let Job = ../type/Job.dhall
let GHCVersion = ../type/GHCVersion.dhall
let NixShell = ../type/NixShell.dhall

let Args =
  { Type =
      { ghcVersion: GHCVersion.Type
      , nixShell: NixShell.Type
      , steps: List Step.Type
      , runsOn: List Job.RunsOn
      , nixExtraArgs: Optional Text
      }
  , default =
      { ghcVersion = GHCVersion.Type.ghc88
      , nixShell = NixShell.Type.dev-shell-optimized
      , nixExtraArgs = None Text
      }
  }

let createJob =
  \(args: Args.Type) ->
    let nixExtraArgs = merge
      { None = ""
      , Some = \(args: Text) ->
          "${args} "
      }
      args.nixExtraArgs

    let wrapCommand = \(cmd: Text) ->
      ''
      nix-shell -j8 ${nixExtraArgs}\
        -A ${GHCVersion.format args.ghcVersion}.${NixShell.format args.nixShell} \
        release.nix \
        --run "${cmd}"
      ''

    let wrapStep = \(step: Step.Type) ->
          step //
          { run = optionalMap Text Text wrapCommand step.run }

    let wrappedSteps = listMap Step.Type Step.Type wrapStep args.steps

    let steps =
      [ Step ::
          { name = "Checkout Code"
          , uses = Some "actions/checkout@v2"
          }
      , Step ::
          { name = "Setup Nix"
          , uses = Some "cachix/install-nix-action@v10"
          }
      , Step ::
          { name = "Setup Cachix"
          , uses = Some "cachix/cachix-action@v6"
          , with = Some toMap
              { name = "scrive"
              , authToken = "\${{ secrets.CACHIX_AUTH_TOKEN }}"
              }
          }
      , Step ::
          { name = "Setup SSH"
          , env = Some toMap
              { SSH_PRIVATE_KEY = "\${{ secrets.SSH_PRIVATE_KEY }}"
              }
          , run = Some ''
              eval $(ssh-agent)
              ssh-add - <<< "''${{ secrets.SSH_PRIVATE_KEY }}"
              echo "::set-env name=SSH_AUTH_SOCK::$SSH_AUTH_SOCK"
              ''
          }
      , Step ::
          { name = "Load Nix Shell"
          , run = Some (wrapCommand "echo Loaded Nix shell")
          }

      ] # wrappedSteps
    in
    Job.Job ::
      { runs-on = args.runsOn
      , steps = steps
      }
in
{ createJob = createJob
, Args = Args
}
