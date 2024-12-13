MAKEFLAGS += --no-builtin-rules
.SUFFIXES:
.DEFAULT_GOAL := help

## Test all solutions
.PHONY: solve
solve: opt
	stack exec advent2024

day ?= 01
part ?= 1

## Test a solution for one day by setting [day=N] [part=N]
.PHONY: run
run: opt
	stack exec advent2024 -- -m $(day)/$(part)

## Build optimized
.PHONY: opt
opt:
	stack build advent2024 --pedantic --interleaved-output

## Build unoptimized
.PHONY: build
build:
	stack build advent2024 --pedantic --fast --interleaved-output

## Build optimized and dump final core
.PHONY: core
core:
	stack build advent2024 --pedantic --interleaved-output --ghc-options='-ddump-to-file -ddump-simpl'
	find . -name 'Day*.dump-simpl' | sort -h

## Update compiler, dependencies, and tools
.PHONY: update
update:
	$(MAKE) update.stack update.tools

## Set up the compiler and project dependencies
.PHONY: update.stack
update.stack:
	stack setup
	stack build --fast --dependencies-only

## Install additional tooling
.PHONY: update.tools
update.tools:
	stack build --copy-compiler-tool hspec-discover

## Clean project
.PHONY: clean
clean:
	stack clean advent2024

# Produce help output for Makefile
#
# Doc blocks begin with ##
#
# Sections can be added with ## -- Section --
#
# source: https://gist.github.com/prwhite/8168133#gistcomment-2749866
#
.PHONY: help
help:
	@printf "Usage\n";
	@awk '{ \
	  if ($$0 ~ /^.PHONY: [a-zA-Z\/\-\.\_0-9]+$$/) { \
	    helpCommand = substr($$0, index($$0, ":") + 2); \
	    if (helpMessage) { \
	      printf "\033[36m%-20s\033[0m %s\n", \
	        helpCommand, helpMessage; \
	      helpMessage = ""; \
	    } \
	  } else if ($$0 ~ /^[a-zA-Z\/\-\.\_0-9.]+:/) { \
	    helpCommand = substr($$0, 0, index($$0, ":")); \
	    if (helpMessage) { \
	      printf "\033[36m%-20s\033[0m %s\n", \
	        helpCommand, helpMessage; \
	      helpMessage = ""; \
	    } \
	  } else if ($$0 ~ /^##/) { \
	    if (helpMessage) { \
	      helpMessage = helpMessage"\n                     "substr($$0, 3); \
	    } else { \
	      helpMessage = substr($$0, 3); \
	    } \
	  } else { \
	    if (helpMessage) { \
	      print "\n                     "helpMessage"\n" \
	    } \
	    helpMessage = ""; \
	  } \
	}' \
	$(MAKEFILE_LIST)
