AGENTS.md: Guidelines for Agentic Coding in org-trello-ng
======================================================================

## Project Overview

`org-trello-ng` is a next-generation Emacs Lisp minor mode for integrating `org-mode` with Trello. It supports CRUD operations on Trello cards (as org tasks), syncing org buffers to Trello boards, improved performance for large boards (>100 cards), card moving between boards (like `org-refile` using `org-refile-targets`), and a modern test suite for Emacs 30+.

**Prioritize**: Robust CRUD sync, batch API calls for performance, *refile* command, working ERT tests with real/mocked API calls gated by user token.

## Directory Structure

- `src/`: Emacs Lisp source files (e.g., org-trello-ng.el for minor mode, api.el for REST wrappers, sync.el for bidirectional logic, commands.el for CRUD/refile cmds)
- `test/`: ERT test files
- `.beads/`: Managed by Beads for issues (do not modify directly)

## Coding Guidelines

- Follow Emacs Lisp conventions: Use `defun`, `defvar`, etc.; prefix functions/vars with `org-trello-ng-`.
- Mimic existing patterns; check for libraries via neighboring files or package requires.
- Byte-compile all `.el` files (e.g., via `emacs --batch --eval "(byte-compile-file \"file.el\")"`).
- Use `checkdoc` for style checking.
- Avoid deprecated functions in Emacs 30+; ensure compatibility.
- Terse comments; function and var names should convey intent.
- For API: Wrap Trello REST calls in `src/api.el` using `request` or similar; handle errors.
- Security: Use auth-source for tokens from `~/.authinfo.gpg` (machine "api.trello.com"); never log secrets.
- For sync: Optimize with batching; be mindful of [Trello rate limits](https://developer.atlassian.com/cloud/trello/guides/rest-api/rate-limits/) (300 req/10s per key, 100 req/10s per token, 429 on limit).

## Testing

- `Makefile` defines recipes `build`, `test`, `lint` useful for testing
- After changes, run `make lint` to check `elisp` syntax
- After every change, run `make all` to test for regressions
- All new functionality require tests under `test/` directory
- `make test` uses ERT i.e. `emacs -batch -l ert -l test/file.el -f ert-run-tests-batch-and-exit`
- Gate real API tests with user token; provide mocks for offline/safety.

<!-- BEGIN BEADS INTEGRATION -->
## Issue Tracking with bd (beads)

This project uses **bd (beads)** for ALL issue tracking. Do NOT use markdown TODOs or external trackers.
Pull latest changes from `main` branch and then create a new feature branch based on the beads issue name + short semantic description.


### Quick Reference

```bash
bd ready --json                        # Find unblocked work
bd update <id> --claim --json          # Claim a task
bd create "Title" -t task -p 2 --json  # Create issue (types: bug/feature/task/epic/chore)

# Use stdin for descriptions with special characters (backticks, !, nested quotes)
echo 'Description with `backticks` and "quotes"' | bd create "Title" --description=- --json

bd close <id> --reason "Done" --json   # Complete work
```

Priorities: 0=critical, 1=high, 2=medium (default), 3=low, 4=backlog

Link discovered work: `bd create "Found bug" -p 1 --deps discovered-from:<parent-id>`

### Dolt Remote & Sync

The task database syncs to [DoltHub](https://www.dolthub.com/repositories/gojun077/org_trello_ng) independently of git. The remote URL is also recorded in `.beads/config.yaml` under `dolt.remote`.

```bash
bd dolt status                         # Check Dolt server status
bd dolt push                           # Push task data to DoltHub
bd dolt pull                           # Pull latest task data (do this at session start)
bd dolt commit                         # Commit pending changes locally
bd dolt remote list                    # List configured remotes
```

**Useful flags**: `--readonly` (block writes), `--sandbox` (disable auto-sync), `--dolt-auto-commit batch` (defer commits). Run `bd dolt --help` for full reference.

### Shared Dolt Server (multi-worktree)

All git worktrees share a **single** dolt server instance hosted by the main repo on port `3307`. This ensures that `bd update <id> --claim` is transactionally visible across all concurrent agent sessions.

- **Main repo** owns the dolt server and data directory (`.beads/dolt/`).
- **Worktrees** connect to the main repo's server; they do **not** run their own.
- `make setup` auto-detects whether it is running in the main repo or a worktree and configures accordingly.
- **Always run `make setup` in the main repo first** before setting up any worktree.
- Do **NOT** run `bd dolt start` or `bd dolt stop` from a worktree.

### Workspace Bootstrap (fresh clone or git worktree)

Run `make setup` to bootstrap beads/dolt. It handles dolt init, remote configuration, data fetch, and server restart. In a worktree, it connects to the main repo's shared dolt server instead.

Requires `dolt` and `bd` on `PATH`; will prompt `dolt login` if no DoltHub credentials exist.

```bash
bd ready --json          # If this works, bootstrap is already done
make setup               # Otherwise, run this (main repo FIRST, then worktrees)
```

### Troubleshooting

- **`bd ready` fails with "database not found"**: Re-run `make setup` (main repo first, then worktree).
- **`bd dolt test` fails in a worktree**: The shared dolt server in the main repo is not running. Run `make setup` in the main repo to start it.
- **`bd dolt push` fails with "no common ancestor" or "PermissionDenied"**:
  1. Verify remote: `cd .beads/dolt/org_trello_ng && dolt remote -v`
  2. Force-push via SQL: `mysql -h 127.0.0.1 -P <port> -u root -e "CALL dolt_push('--force', 'origin', 'main');" org_trello_ng`
  3. After that, `bd dolt push` should work normally.
<!-- END BEADS INTEGRATION -->

## Session Initialization (The Lobby)

Each git worktree (e.g. `org-trello-ng-worker-0`, `-1`, `-2`) starts from a detached `HEAD` on `main`. This is expected. **Do not commit on detached HEAD.**

1. **Verify the worktree is clean:**
   ```bash
   git status --porcelain
   ```
   If the worktree is dirty, stop and resolve before claiming new work.

2. **Sync task state and git refs:**
   ```bash
   bd dolt pull
   git fetch origin --prune
   ```
   If `bd ready` fails, run `make setup` first.

3. **Pick and claim exactly one task:**
   ```bash
   bd ready --json
   bd update <id> --claim --json
   ```

4. **Create or resume the task branch** (handles local, remote, and new branches):
   ```bash
   make branch TASK=<id> SLUG=<short-slug>
   ```
   If Git reports the branch is already checked out in another worktree, do **not** force a new branch. Inspect `git worktree list` and resume from the existing worktree instead.

5. **Confirm you are on a branch before editing:**
   ```bash
   git branch --show-current
   ```

## Session Completion

**Work is NOT complete until task data is synced, the branch is pushed, and a PR exists.** Never stop before all three succeed.

Run the full completion sequence in one step:
```bash
make finish BEADS_ID=<id> PR_DESCRIPTION="A concise summary of what was changed and why"
```

The `PR_DESCRIPTION` variable is **required** and must be written by the agent — it should be a clear, human-readable summary of the changes (not pulled from bead metadata). The PR body will also include the bead task title and a list of commits automatically.

This runs quality gates (`make all`), rebases onto `origin/main`, pushes task data and code, creates or reuses a GitHub PR (idempotent), and closes the bead — in that order. If any step fails, it stops.

Or run steps individually:

1. `make all` — run quality gates
2. `git fetch origin --prune && git rebase origin/main`
3. `bd dolt push && git push -u origin HEAD`
4. `make pr BEADS_ID=<id> PR_DESCRIPTION="summary of changes"` — create or reuse the PR (idempotent)
5. `bd close <id> --reason "Done" --json` — only after push and PR succeed
6. Hand off context for the next session, including bead id, branch name, and PR URL
7. File issues (`bd create`) for remaining or discovered follow-up work
   - verify that the issue was created with `bd list --id`
8. **Close parent epic if all sub-tasks are done:**
   ```bash
   bd show <epic-id> --json  # Check epic_closeable field
   bd close <epic-id> --reason "All sub-tasks complete" --json
   ```
