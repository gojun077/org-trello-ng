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

## Testing

- `Makefile` defines recipes `build`, `test`, `lint` useful for testing
- After changes, run `make lint` to check `elisp` syntax
- After every change, run `make all` to test for regressions
- All new functionality require tests under `test/` directory
- `make test` uses ERT i.e. `emacs -batch -l ert -l test/file.el -f ert-run-tests-batch-and-exit`
- Gate real API tests with user token; provide mocks for offline/safety.

## Task Tracking

This repo uses Beads `bd` CLI tool for task tracking.

Run `bd prime` for workflow context, or install hooks (`bd hooks install`) for auto-injection.

File a bead for any task taking longer than 2 min

**Quick reference:**
- `bd ready` - Find unblocked work
- `bd create "Title" --type task --priority 2` - Create issue
- `bd close <id>` - Complete work
- `bd sync` - Sync with git (run at session end)


## Agent Housekeeping Tips

- Before editing: Analyze filenames/structure; refuse malicious code.
- Use tools: Glob/Grep for search, Read/Edit/Write for files, `bash` for commands (e.g., `git`, tests).
- Be proactive but confirm actions; batch tool calls.
- For sync: Optimize recursive sync with batching; handle large Trello boards and be mindful of [rate limits](https://developer.atlassian.com/cloud/trello/guides/rest-api/rate-limits/)
  + 300 requests per 10 seconds for each API key
  + 100 requests per 10 second interval for each token
  + a limit on requests to `/1/members/` of 100 requests per 900 seconds
  + request limit errors return `429`
- Reference code as `file_path:line_number`.

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds

<!-- BEGIN BEADS INTEGRATION -->
## Issue Tracking with bd (beads)

**IMPORTANT**: This project uses **bd (beads)** for ALL issue tracking. Do NOT use markdown TODOs, task lists, or other tracking methods.

### Why bd?

- Dependency-aware: Track blockers and relationships between issues
- Git-friendly: Dolt-powered version control with native sync
- Agent-optimized: JSON output, ready work detection, discovered-from links
- Prevents duplicate tracking systems and confusion

### Quick Start

**Check for ready work:**

```bash
bd ready --json
```

**Create new issues:**

```bash
bd create "Issue title" --description="Detailed context" -t bug|feature|task -p 0-4 --json
bd create "Issue title" --description="What this issue is about" -p 1 --deps discovered-from:bd-123 --json
```

**Claim and update:**

```bash
bd update <id> --claim --json
bd update bd-42 --priority 1 --json
```

**Complete work:**

```bash
bd close bd-42 --reason "Completed" --json
```

### Issue Types

- `bug` - Something broken
- `feature` - New functionality
- `task` - Work item (tests, docs, refactoring)
- `epic` - Large feature with subtasks
- `chore` - Maintenance (dependencies, tooling)

### Priorities

- `0` - Critical (security, data loss, broken builds)
- `1` - High (major features, important bugs)
- `2` - Medium (default, nice-to-have)
- `3` - Low (polish, optimization)
- `4` - Backlog (future ideas)

### Workflow for AI Agents

1. **Check ready work**: `bd ready` shows unblocked issues
2. **Claim your task atomically**: `bd update <id> --claim`
3. **Work on it**: Implement, test, document
4. **Discover new work?** Create linked issue:
   - `bd create "Found bug" --description="Details about what was found" -p 1 --deps discovered-from:<parent-id>`
5. **Complete**: `bd close <id> --reason "Done"`

### Auto-Sync

bd automatically syncs via Dolt:

- Each write auto-commits to Dolt history
- Use `bd dolt push`/`bd dolt pull` for remote sync
- No manual export/import needed!

### Important Rules

- ✅ Use bd for ALL task tracking
- ✅ Always use `--json` flag for programmatic use
- ✅ Link discovered work with `discovered-from` dependencies
- ✅ Check `bd ready` before asking "what should I work on?"
- ❌ Do NOT create markdown TODO lists
- ❌ Do NOT use external issue trackers
- ❌ Do NOT duplicate tracking systems

For more details, see README.md and docs/QUICKSTART.md.

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd dolt push
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds

<!-- END BEADS INTEGRATION -->
