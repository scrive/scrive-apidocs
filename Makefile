cachix:
	./nix/scripts/push-cachix.sh

sync-nix:
	./nix/scripts/sync-plan.sh
	nix-shell -A ghc88.manual-shell release.nix --run "cd frontend && ./sync-nix.sh"
	nix-shell -A ghc88.manual-shell release.nix --run "cd frontend-elm && ./sync-nix.sh"

sync-workflow:
	nix-shell -p dhall-json --run "./ci/workflow/scripts/generate-workflow.sh"

.PHONY: cachix sync-nix
