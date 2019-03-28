
CP       ?= install -p -m 644
MKDIR    ?= install -p -m 755 -d
RMDIR    ?= rm -rf
TAR      ?= tar
SED      ?= sed

EMACSBIN ?= emacs
BATCH     = $(EMACSBIN) -Q --batch $(LOAD_PATH)

VERSION ?= $(shell test -e $(TOP).git && git describe --tags --abbrev=0 | cut -c2-)

# ifndef LOAD_PATH
# ...
# LOAD_PATH = -L $(TOP)/lisp
# endif
