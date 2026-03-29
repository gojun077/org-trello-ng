# Makefile for org-trello-ng
# Emacs Lisp project build automation

EMACS ?= emacs
BATCH = $(EMACS) --batch -Q

SRC_DIR = src
TEST_DIR = test
SRC_FILES = $(wildcard $(SRC_DIR)/*.el)
TEST_FILES = $(wildcard $(TEST_DIR)/*.el)
COMPILED = $(SRC_FILES:.el=.elc)

.PHONY: all build test lint clean setup branch pr finish help

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
DOLT_PORT   := 3307

# Derive the main repo root from git's common dir (works in worktrees too)
MAIN_REPO   := $(shell git rev-parse --git-common-dir | sed 's|/\.git$$||')
IS_WORKTREE := $(shell [ "$$(git rev-parse --git-common-dir)" = ".git" ] && echo no || echo yes)

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
ifeq ($(IS_WORKTREE),yes)
	@echo "==> Worktree detected. Connecting to shared dolt server in main repo..."
	@echo "    Main repo: $(MAIN_REPO)"
	bd dolt set port $(DOLT_PORT)
	bd dolt set data-dir $(MAIN_REPO)/$(DOLT_DIR)
	@echo "==> Testing connection to shared dolt server..."
	@bd dolt test || { echo "ERROR: Cannot reach shared dolt server. Run 'make setup' in the main repo first."; exit 1; }
else
	@echo "==> Main repo detected. Initializing dolt server root in $(DOLT_DIR)..."
	@mkdir -p $(DOLT_DIR)
	@if [ ! -d "$(DOLT_DIR)/.dolt" ]; then \
		cd $(DOLT_DIR) && dolt init --name "org-trello-ng" --email "noreply@org-trello-ng"; \
	else \
		echo "    $(DOLT_DIR)/.dolt already exists, skipping init."; \
	fi
	@echo "==> Initializing database repo in $(DOLT_DIR)/$(DOLT_DB)..."
	@mkdir -p $(DOLT_DIR)/$(DOLT_DB)
	@if [ ! -d "$(DOLT_DIR)/$(DOLT_DB)/.dolt" ]; then \
		cd $(DOLT_DIR)/$(DOLT_DB) && dolt init --name "org-trello-ng" --email "noreply@org-trello-ng"; \
	else \
		echo "    $(DOLT_DIR)/$(DOLT_DB)/.dolt already exists, skipping init."; \
	fi
	@echo "==> Configuring dolt remote on database repo..."
	@cd $(DOLT_DIR)/$(DOLT_DB) && \
		if ! dolt remote -v 2>/dev/null | grep -q origin; then \
			dolt remote add origin $(DOLT_REMOTE); \
			echo "    Added remote 'origin'."; \
		else \
			echo "    Remote 'origin' already configured."; \
		fi
	@echo "==> Fetching data from DoltHub and resetting to remote/main..."
	@cd $(DOLT_DIR)/$(DOLT_DB) && dolt fetch origin && dolt reset --hard remotes/origin/main
	@echo "==> Pinning dolt server port to $(DOLT_PORT)..."
	bd dolt set port $(DOLT_PORT) --update-config
	@echo "==> Restarting dolt server via bd..."
	@bd dolt stop  2>/dev/null || true
	@bd dolt start
endif
	@echo "==> Running bd doctor --fix..."
	@bd doctor --fix --yes
	@echo "==> Configuring beads role..."
	@bd config set beads.role maintainer
	@echo "==> Verifying setup..."
	@bd ready --json >/dev/null 2>&1 && echo "    bd ready: OK" || echo "    WARNING: bd ready returned no tasks (may be expected)"
	@echo ""
	@echo "Setup complete! You can now use 'bd' commands."
	@echo "Run 'bd ready --json' to see available tasks."

# --- Session workflow targets ---

# Create or resume a task branch. Usage: make branch TASK=<id> SLUG=<short-slug>
branch:
ifndef TASK
	$(error TASK is required. Usage: make branch TASK=<id> SLUG=<short-slug>)
endif
ifndef SLUG
	$(error SLUG is required. Usage: make branch TASK=<id> SLUG=<short-slug>)
endif
	@BRANCH="bd/$(TASK)-$(SLUG)"; \
	if git show-ref --verify --quiet "refs/heads/$$BRANCH"; then \
		echo "Switching to existing local branch $$BRANCH"; \
		git switch "$$BRANCH"; \
	elif git ls-remote --exit-code --heads origin "$$BRANCH" >/dev/null 2>&1; then \
		echo "Tracking remote branch $$BRANCH"; \
		git switch -c "$$BRANCH" --track "origin/$$BRANCH"; \
	else \
		echo "Creating new branch $$BRANCH from origin/main"; \
		git switch -c "$$BRANCH" origin/main; \
	fi
	@echo ""
	@echo "On branch: $$(git branch --show-current)"

# Create or reuse a GitHub PR. Usage: make pr BEADS_ID=<id> PR_DESCRIPTION="summary of changes"
pr:
ifndef BEADS_ID
	$(error BEADS_ID is required. Usage: make pr BEADS_ID=<id>)
endif
	@BRANCH=$$(git branch --show-current); \
	if [ -z "$$BRANCH" ]; then \
		echo "ERROR: not on a branch (detached HEAD?)"; exit 1; \
	fi; \
	PR_URL=$$(gh pr list --head "$$BRANCH" --state all --json url --jq '.[0].url'); \
	if [ -n "$$PR_URL" ]; then \
		echo "PR already exists: $$PR_URL"; \
	else \
		LABEL_ARGS=""; \
		if gh label list --json name --jq '.[].name' | grep -qx 'agent-pr'; then \
			LABEL_ARGS="--label agent-pr"; \
		else \
			echo "WARNING: label 'agent-pr' not found on remote; creating PR without it"; \
		fi; \
		TASK_TITLE=$$(bd show "$(BEADS_ID)" --json | jq -r '.[0].title // empty'); \
		COMMIT_LOG=$$(git log origin/main..HEAD --reverse --pretty='- %s' 2>/dev/null); \
		PR_BODY="## Beads Task: $(BEADS_ID)"; \
		if [ -n "$$TASK_TITLE" ]; then \
			PR_BODY="$$PR_BODY$$(printf '\n\n### Task\n')$$TASK_TITLE"; \
		fi; \
		PR_BODY="$$PR_BODY$$(printf '\n\n### Description\n')$(PR_DESCRIPTION)"; \
		if [ -n "$$COMMIT_LOG" ]; then \
			PR_BODY="$$PR_BODY$$(printf '\n\n### Commits\n')$$COMMIT_LOG"; \
		fi; \
		PR_BODY="$$PR_BODY$$(printf '\n\n---\n*Generated by Agent in Worktree: ')$$(hostname)*"; \
		gh pr create \
			--base main \
			--draft \
			$$LABEL_ARGS \
			--title "Fix($(BEADS_ID)): $$(git log -1 --pretty=%s)" \
			--body "$$PR_BODY"; \
	fi

# Full session completion. Usage: make finish BEADS_ID=<id> PR_DESCRIPTION="summary of changes"
finish:
ifndef BEADS_ID
	$(error BEADS_ID is required. Usage: make finish BEADS_ID=<id>)
endif
	@echo "==> Running quality gates..."
	@$(MAKE) all
	@echo "==> Rebasing onto origin/main..."
	git fetch origin --prune
	git rebase origin/main
	@echo "==> Pushing task data and code..."
	bd dolt push
	git push -u origin HEAD
	@echo "==> Creating/reusing PR..."
	@$(MAKE) pr BEADS_ID=$(BEADS_ID) PR_DESCRIPTION=$(PR_DESCRIPTION)
	@echo "==> Closing bead $(BEADS_ID)..."
	bd close $(BEADS_ID) --reason "Done" --json
	@echo ""
	@echo "Session complete. Branch: $$(git branch --show-current)"
	@echo ""
	@echo "ACTION REQUIRED: File issues for any remaining or discovered follow-up work:"
	@echo "  bd create \"<title>\" -t <type> -p <priority> --json"
	@echo "  Verify with: bd list --id"

help:
	@echo "Available targets:"
	@echo "  all    - Build and test (default)"
	@echo "  build  - Byte-compile source files"
	@echo "  test   - Run ERT tests"
	@echo "  lint   - Run checkdoc on source files"
	@echo "  clean  - Remove compiled files"
	@echo "  setup  - Bootstrap beads/dolt for fresh clones or worktrees"
	@echo "  branch - Create/resume task branch (TASK=<id> SLUG=<slug>)"
	@echo "  pr     - Create/reuse GitHub PR (BEADS_ID=<id> PR_DESCRIPTION=\"...\")"
	@echo "  finish - Full session completion (BEADS_ID=<id> PR_DESCRIPTION=\"...\")"
	@echo ""
	@echo "Variables:"
	@echo "  EMACS    - Emacs executable (default: emacs)"
	@echo "  TASK     - Beads task ID (for branch target)"
	@echo "  SLUG     - Short description slug (for branch target)"
	@echo "  BEADS_ID - Beads task ID (for pr/finish targets)"
