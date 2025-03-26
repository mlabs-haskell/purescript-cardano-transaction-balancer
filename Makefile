SHELL := bash
.ONESHELL:
.PHONY: check-format format build
.SHELLFLAGS := -eu -o pipefail -c

ps-sources := $(shell fd --no-ignore-parent -epurs)
nix-sources := $(shell fd --no-ignore-parent -enix --exclude='spago*')
purs-args := "--stash --censor-lib --censor-codes=UserDefinedWarning,ImplicitImport,ImplicitQualifiedImport,ImplicitQualifiedImportReExport"


.ONESHELL:
check-explicit-exports:
	@if grep -rn '(\.\.)' ${ps-sources}; then
		echo "Use explicit imports/exports ^"
		echo "Run ./scripts/import-fixer.sh to autofix some of these"
		exit 1
	else
		echo "All imports/exports are explicit"
	fi

check-format: check-explicit-exports
	@purs-tidy check ${ps-sources}
	@nixpkgs-fmt --check ${nix-sources}

format:
	@purs-tidy format-in-place ${ps-sources}
	nixpkgs-fmt ${nix-sources}
	make check-explicit-exports

build:
	@spago build --purs-args ${purs-args}
