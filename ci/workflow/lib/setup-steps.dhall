let Step = ../type/Step.dhall
let Json = ../type/Json.dhall
let config = ../config.dhall

let checkout-step = Step ::
  { name = "Checkout Code"
  , uses = Some "actions/checkout@v2"
  }

let nix-step = Step ::
  { name = "Setup Nix"
  , run = Some ''
      ./ci/workflow/scripts/setup-nix.sh
      ''
  }

let cachix-step = Step ::
  { name = "Setup Cachix"
  , run = Some ''
      if ! type cachix
      then
        nix-env -iA cachix -f https://cachix.org/api/v1/install
      fi

      cachix authtoken ''${{ secrets.CACHIX_AUTH_TOKEN }}
      cachix use scrive
      cachix use iohk
      ''
  }

let ssh-step = Step ::
  { name = "Setup SSH"
  , env = Some (toMap
      { SSH_KEY_PDFTOOLS = "\${{ secrets.SSH_KEY_PDFTOOLS }}"
      })
  , run = Some "./ci/workflow/scripts/setup-ssh.sh"
  }

let install-steps =
  [ nix-step
  , cachix-step
  , ssh-step
  ]

let setup-steps =
  [ checkout-step
  ] # install-steps

let nix-gc-step = Step ::
  { name = "Nix Collect Garbage"
  , run = Some ''
      df -h
      nix-collect-garbage
      df -h
      ''
  }

-- If config.nix-collect-garbage is true, this means we try to run
-- nix-collect-garbage at the beginning of a job in order to avoid
-- the self hosted runner at AWS Fargate from running out of disk
-- space.
let nix-shell-step =
  if config.nix-collect-garbage
  then \(shell: Text) -> Step ::
    { name = "Load Nix Shell"
    , run = Some ''
        df -h
        nix-collect-garbage
        rm -rf ~/.cabal
        df -h

        nix-shell -j4 -A ${shell} release.nix --run true
        ''
    }

  else \(shell: Text) -> Step ::
    { name = "Load Nix Shell"
    , run = Some ''
        nix-shell -j4 -A ${shell} release.nix --run true
        ''
    }

in
{ setup-steps = setup-steps
, install-steps = install-steps
, checkout-step = checkout-step
, nix-step = nix-step
, cachix-step = cachix-step
, ssh-step = ssh-step
, nix-gc-step = nix-gc-step
, nix-shell-step = nix-shell-step
}
