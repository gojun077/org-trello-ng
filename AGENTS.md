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

### Fresh Clone Bootstrap

On a fresh clone the local dolt repo under `.beads/dolt/` has no remote and no data. Follow these steps:

1. **Authenticate with DoltHub** (opens browser for OAuth):
   ```bash
   dolt login
   ```
2. **Add the remote and fetch data** (run from `.beads/dolt/`):
   ```bash
   cd .beads/dolt
   dolt remote add origin https://doltremoteapi.dolthub.com/gojun077/org_trello_ng
   dolt fetch origin
   dolt reset --hard remotes/origin/main
   ```
   `dolt pull` will fail with "no common ancestor" on a fresh init — use fetch+reset instead.
3. **Restart the dolt server** so `bd` picks up the new data:
   ```bash
   bd dolt stop && bd dolt start
   ```
4. **Run doctor and fix warnings**:
   ```bash
   bd doctor --fix --yes
   bd config set beads.role maintainer
   bd vc commit -m "bootstrap: initial setup"
   ```
5. **Verify** everything works:
   ```bash
   bd ready --json                    # Should list tasks from remote
   bd dolt push                       # Should succeed
   ```

### Troubleshooting

- **`bd dolt push` fails with "no common ancestor" or "PermissionDenied"**:
  1. Verify the CLI-level remote exists: `dolt remote -v` (run from `.beads/dolt/`)
  2. If missing, add it: `dolt remote add origin <url>` (get URL from `bd dolt remote list`)
  3. Force-push through SQL to reconcile: `mysql -h 127.0.0.1 -P <port> -u root -e "CALL dolt_push('--force', 'origin', 'main');" <db>`
  4. After that, `bd dolt push` should work normally.
- **`bd` can't find the database** ("database not found on Dolt server"): The dolt server names the database after the directory (i.e. `org_trello_ng`). Ensure `dolt.database` in `.beads/config.yaml` is set to `"org_trello_ng"`. Fix with: `bd dolt set database org_trello_ng --update-config`
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
