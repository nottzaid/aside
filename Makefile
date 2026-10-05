EMACS ?= emacs
AGENTS ?= opencode claude codex cline
# Extra directories to load from, such as Evil's, to include its tests.
LOAD_PATH ?=
BATCH = $(EMACS) --batch -Q -L . -L test $(foreach dir,$(LOAD_PATH),-L $(dir))

.PHONY: all compile test live record screenshots clean

all: compile test

# Byte-compile everything, treating warnings as errors.
compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile aside*.el test/*.el
	@rm -f *.elc test/*.elc

# Replay recorded agent sessions through the real popup.  Fast; no quota.
test:
	$(BATCH) -l test/aside-test.el -f ert-run-tests-batch-and-exit

# Run the same flow against the real agents.  Costs a little quota.
live:
	ASIDE_LIVE="$(AGENTS)" $(BATCH) -l test/aside-live-test.el \
	  --eval '(ert-run-tests-batch-and-exit (quote (tag live)))'

# Re-record an agent's transcripts: make record AGENT=opencode
record:
	$(BATCH) -l test/aside-record.el -f aside-record $(AGENT)

# Redraw docs/*.png in a graphical Emacs on a virtual X display.
screenshots:
	xvfb-run -a -s '-screen 0 1920x1080x24' env -u WAYLAND_DISPLAY \
	  $(EMACS) -Q -L . -L test -l test/aside-screenshots.el -f aside-shots

clean:
	rm -f *.elc test/*.elc
