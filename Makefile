# Makefile for org-trello-ng
# Emacs Lisp project build automation

EMACS ?= emacs
BATCH = $(EMACS) --batch -Q

SRC_DIR = src
TEST_DIR = test
SRC_FILES = $(wildcard $(SRC_DIR)/*.el)
TEST_FILES = $(wildcard $(TEST_DIR)/*.el)
COMPILED = $(SRC_FILES:.el=.elc)

.PHONY: all build test lint clean setup help

all: build test

build: $(COMPILED)

%.elc: %.el
	$(BATCH) -L $(SRC_DIR) --eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $<

test:
	$(BATCH) -L $(SRC_DIR) -L $(TEST_DIR) \
		$(foreach f,$(TEST_FILES),-l $(f)) \
		-f ert-run-tests-batch-and-exit

lint:
	@for f in $(SRC_FILES); do \
		echo "Checking $$f..."; \
		$(BATCH) -L $(SRC_DIR) -l $$f --eval "(checkdoc-file \"$$f\")" || exit 1; \
	done

clean:
	rm -f $(SRC_DIR)/*.elc $(TEST_DIR)/*.elc

# Dolt / Beads configuration (read from .beads/config.yaml)
DOLT_REMOTE := https://doltremoteapi.dolthub.com/gojun077/org_trello_ng
DOLT_DB     := org_trello_ng
DOLT_DIR    := .beads/dolt

setup:
	@echo "==> Checking prerequisites..."
	@command -v dolt  >/dev/null 2>&1 || { echo "ERROR: dolt not found. Install from https://docs.dolthub.com/introduction/installation"; exit 1; }
	@command -v bd    >/dev/null 2>&1 || { echo "ERROR: bd (beads) not found."; exit 1; }
	@echo "==> Ensuring dolt is authenticated (skipping if creds exist)..."
	@if [ ! -f "$$HOME/.dolt/creds/"*.jwk ] 2>/dev/null; then \
		echo "No DoltHub credentials found — running 'dolt login' (opens browser)..."; \
		dolt login; \
	else \
		echo "    DoltHub credentials found, skipping login."; \
	fi
	@echo "==> Initializing dolt repo in $(DOLT_DIR)..."
	@mkdir -p $(DOLT_DIR)
	@if [ ! -d "$(DOLT_DIR)/.dolt" ]; then \
		cd $(DOLT_DIR) && dolt init --name "org-trello-ng" --email "noreply@org-trello-ng"; \
	else \
		echo "    $(DOLT_DIR)/.dolt already exists, skipping init."; \
	fi
	@echo "==> Configuring dolt remote..."
	@cd $(DOLT_DIR) && \
		if ! dolt remote -v 2>/dev/null | grep -q origin; then \
			dolt remote add origin $(DOLT_REMOTE); \
			echo "    Added remote 'origin'."; \
		else \
			echo "    Remote 'origin' already configured."; \
		fi
	@echo "==> Fetching data from DoltHub and resetting to remote/main..."
	@cd $(DOLT_DIR) && dolt fetch origin && dolt reset --hard remotes/origin/main
	@echo "==> Restarting dolt server via bd..."
	@bd dolt stop  2>/dev/null || true
	@bd dolt start
	@echo "==> Running bd doctor --fix..."
	@bd doctor --fix --yes
	@echo "==> Configuring beads role..."
	@bd config set beads.role maintainer
	@echo "==> Verifying setup..."
	@bd ready --json >/dev/null 2>&1 && echo "    bd ready: OK" || echo "    WARNING: bd ready returned no tasks (may be expected)"
	@echo ""
	@echo "Setup complete! You can now use 'bd' commands."
	@echo "Run 'bd ready --json' to see available tasks."

help:
	@echo "Available targets:"
	@echo "  all    - Build and test (default)"
	@echo "  build  - Byte-compile source files"
	@echo "  test   - Run ERT tests"
	@echo "  lint   - Run checkdoc on source files"
	@echo "  clean  - Remove compiled files"
	@echo "  setup  - Bootstrap beads/dolt for fresh clones or worktrees"
	@echo ""
	@echo "Variables:"
	@echo "  EMACS  - Emacs executable (default: emacs)"
