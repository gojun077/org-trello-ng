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

### Workspace Bootstrap (fresh clone or git worktree)

New workspaces — whether created via `git clone` or `git worktree add` — have a `.beads/dolt/` directory with an empty Dolt repo (no remote, no data). The Dolt server may already be running but it won't find the database. **You must bootstrap before `bd ready` will work.**

#### Quick diagnosis

```bash
bd ready --json          # If this works, skip bootstrap
bd dolt status           # Check if dolt server is running and which port/data dir
```

If `bd ready` fails with `"database not found on Dolt server"`, follow the steps below.

#### Step-by-step bootstrap

1. **Check if a remote already exists** (run from `.beads/dolt/`):
   ```bash
   cd .beads/dolt && dolt remote -v
   ```
   If no remote is listed, add one:
   ```bash
   dolt remote add origin https://doltremoteapi.dolthub.com/gojun077/org_trello_ng
   ```
   (If `dolt login` hasn't been run on this machine, do that first.)

2. **Fetch and reset to remote data** (still in `.beads/dolt/`):
   ```bash
   dolt fetch origin
   dolt reset --hard remotes/origin/main
   ```
   > **Note:** `dolt pull` will fail with "no common ancestor" on a fresh init — always use `fetch` + `reset --hard` instead.

3. **Fix the database name mapping** — this is the most common cause of "database not found" errors. The Dolt server names the database after the directory, but on worktrees/clones the mapping may be missing:
   ```bash
   bd dolt set database org_trello_ng --update-config
   ```
   This updates both `metadata.json` and `dolt.database` in `.beads/config.yaml`.

4. **Restart the Dolt server** to pick up the new data and name:
   ```bash
   bd dolt stop && bd dolt start
   ```

5. **Verify** — `bd ready` should now return tasks:
   ```bash
   bd ready --json                    # Should list tasks from remote
   bd dolt push                       # Should succeed
   ```

> **TL;DR one-liner** (from the workspace root, if `dolt login` is already done):
> ```bash
> cd .beads/dolt && dolt remote add origin https://doltremoteapi.dolthub.com/gojun077/org_trello_ng && dolt fetch origin && dolt reset --hard remotes/origin/main && cd ../.. && bd dolt set database org_trello_ng --update-config && bd dolt stop && bd dolt start && bd ready --json
> ```

#### Note on `bd doctor --fix`

`bd doctor --fix --yes` does **not** resolve the "database not found" error. It can fix file permissions and other minor issues, but the remote-add / fetch / database-name steps above must be done manually.

### Troubleshooting

- **`bd ready` / any `bd` command fails with "database not found on Dolt server"**:
  1. Run the bootstrap steps above — most likely the database name mapping is missing.
  2. Confirm `dolt.database` in `.beads/config.yaml` is set to `"org_trello_ng"`.
  3. Fix with: `bd dolt set database org_trello_ng --update-config` then restart: `bd dolt stop && bd dolt start`.

- **`bd dolt push` fails with "no common ancestor" or "PermissionDenied"**:
  1. Verify the CLI-level remote exists: `dolt remote -v` (run from `.beads/dolt/`)
  2. If missing, add it: `dolt remote add origin <url>` (get URL from `bd dolt remote list`)
  3. Force-push through SQL to reconcile: `mysql -h 127.0.0.1 -P <port> -u root -e "CALL dolt_push('--force', 'origin', 'main');" <db>`
  4. After that, `bd dolt push` should work normally.
<!-- END BEADS INTEGRATION -->

## Session Completion

**Work is NOT complete until `git push` succeeds.** Never stop before pushing.

1. File issues (`bd create`) for remaining work
2. Run quality gates if code changed: `make all`
3. Update issue status: `bd close <id>`
4. Push everything:
   ```bash
   git pull --rebase
   bd dolt push
   git push
   ```
5. Hand off context for the next session
