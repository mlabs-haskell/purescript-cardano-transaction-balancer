.PHONY: check-explicit-exports check-format format clean build

ps-sources := $(shell fd --no-ignore-parent -epurs)
nix-sources := $(shell fd --no-ignore-parent -enix --exclude='spago*')
purs-args := "--stash --censor-lib --censor-codes=ImplicitImport,ImplicitQualifiedImport,ImplicitQualifiedImportReExport,UserDefinedWarning"

requires-nix-shell:
	@[ "$(IN_NIX_SHELL)" ] || \
		( echo "The '$(MAKECMDGOALS)' target must be run from inside a nix shell, run 'nix develop' first." \
				&& false \
		)

.ONESHELL:
check-explicit-exports:
	@if grep -rn '(\.\.)' ${ps-sources}; then
		echo "Use explicit imports/exports ^"
		echo "Run ./scripts/import-fixer.sh to autofix some of these"
		exit 1
	else
		echo "All imports/exports are explicit"
	fi

check-format: requires-nix-shell check-explicit-exports
	@purs-tidy check ${ps-sources}
	@nixpkgs-fmt --check ${nix-sources}

format: requires-nix-shell
	@echo '1. Formatting PureScript sources:'
	purs-tidy format-in-place ${ps-sources}
	@echo -e '\n2. Formatting Nix sources:'
	nixpkgs-fmt ${nix-sources}

clean:
	rm -r output

build:
	@spago build --purs-args ${purs-args}
