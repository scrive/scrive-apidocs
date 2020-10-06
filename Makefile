cachix:
	./nix/scripts/push-cachix.sh

sync-nix:
	./nix/scripts/sync-plan.sh
	nix-shell -A ghc88.manual-shell release.nix --run "cd frontend && ./sync-nix.sh"
	nix-shell -A ghc88.manual-shell release.nix --run "cd frontend-elm && ./sync-nix.sh"

sync-workflow:
	nix-shell -p dhall-json --run "./ci/workflow/scripts/generate-workflow.sh"

link-new-frontend:
	nix-shell -A ghc88.dev-shell release.nix --run \
		"./new-frontend/link-new-frontend.sh"

sync-new-frontend:
	nix-shell -p nix-prefetch-git -p nodePackages.node2nix --run \
		"./nix/scripts/sync-new-frontend.sh"

.PHONY: cachix sync-nix
