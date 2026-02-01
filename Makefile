# Makefile for org-trello-ng
# Emacs Lisp project build automation

EMACS ?= emacs
BATCH = $(EMACS) --batch -Q

SRC_DIR = src
TEST_DIR = test
SRC_FILES = $(wildcard $(SRC_DIR)/*.el)
TEST_FILES = $(wildcard $(TEST_DIR)/*.el)
COMPILED = $(SRC_FILES:.el=.elc)

.PHONY: all build test lint clean help

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

help:
	@echo "Available targets:"
	@echo "  all    - Build and test (default)"
	@echo "  build  - Byte-compile source files"
	@echo "  test   - Run ERT tests"
	@echo "  lint   - Run checkdoc on source files"
	@echo "  clean  - Remove compiled files"
	@echo ""
	@echo "Variables:"
	@echo "  EMACS  - Emacs executable (default: emacs)"
