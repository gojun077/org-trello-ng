AGENTS.md: Guidelines for Agentic Coding in org-trello-ng
======================================================================

## Project Overview

`org-trello-ng` is a next-generation Emacs Lisp minor mode for integrating `org-mode` with Trello. It supports CRUD operations on Trello cards (as org tasks), bidirectional syncing between org buffers and Trello boards, improved performance for large boards (>100 cards), card moving between boards (like `org-refile` using `org-refile-targets`), and a modern test suite for Emacs 30+.

**Prioritize**: Robust CRUD sync, batch API calls for performance, *refile*
command, working ERT tests with real/mocked API calls gated by user token.

## Directory Structure

- `src/`: Emacs Lisp source files (e.g., org-trello-ng.el for minor mode,
  api.el for REST wrappers, sync.el for bidirectional logic, commands.el
  for CRUD/refile cmds)
- `test/`: ERT test files
- `.beads/`: Managed by Beads for issues (do not modify directly)

## Coding Guidelines

- Follow Emacs Lisp conventions: Use `defun`, `defvar`, etc.; prefix
  functions/vars with `org-trello-ng-`.
- Mimic existing patterns; check for libraries via neighboring files or
  package requires.
- Byte-compile all `.el` files (e.g., via `emacs --batch --eval "(byte-compile-file \"file.el\")"`).
- Use `checkdoc` for style checking.
- Avoid deprecated functions in Emacs 30+; ensure compatibility.
- Terse comments; function and var names should convey intent.
- For API: Wrap Trello REST calls in `src/api.el` using `request` or
  similar; handle errors.
- Security: Use auth-source for tokens from `~/.authinfo.gpg` (machine
  "api.trello.com"); never log secrets.

## Testing

- Use ERT in `test/`; tests verify CRUD, card moves, sync.
- Gate real API tests with user token; provide mocks for offline/safety.
- Run tests via `emacs -batch -l ert -l test/file.el -f ert-run-tests-batch-and-exit`.
- After changes, run `lint/typecheck` if applicable (ask user if unknown).

## Task Tracking

This repo uses Beads `bd` CLI tool for task tracking

Run `bd prime` for workflow context, or install hooks (`bd hooks install`) for auto-injection.

File a bead for any task taking longer than 2 min

**Quick reference:**
- `bd ready` - Find unblocked work
- `bd create "Title" --type task --priority 2` - Create issue
- `bd close <id>` - Complete work
- `bd sync` - Sync with git (run at session end)


## Agent Housekeeping Tips

- Before editing: Analyze filenames/structure; refuse malicious code.
- Use tools: Glob/Grep for search, Read/Edit/Write for files, Bash for
  commands (e.g., git, tests).
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
