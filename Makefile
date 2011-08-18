# Makefile for LFE
# This simple Makefile uses rebar to compile/clean if it
# exists, else does it explicitly.

BINDIR = ebin
SRCDIR = src
INCDIR = include
DOCDIR = doc
EMACSDIR = emacs

VPATH = $(SRCDIR)

ERLCFLAGS = -W0 +debug_info
ERLC = erlc

## The .erl and .beam files
SRCS = $(notdir $(wildcard $(SRCDIR)/*.erl))
EBINS = $(SRCS:.erl=.beam)

## Where we install LFE, in the ERL_LIBS directory.
INSTALLDIR = $(ERL_LIBS)/lfe

.SUFFIXES: .erl .beam

$(BINDIR)/%.beam: %.erl
	$(ERLC) -I $(INCDIR) -o $(BINDIR) $(ERLCFLAGS) $<

$(SRCDIR)/%.erl: %.xrl
	$(ERLC) -o $(SRCDIR) $<

$(SRCDIR)/%.erl: %.yrl
	$(ERLC) -o $(SRCDIR) $<

all: compile docs

.PHONY: compile erlc_compile install docs clean

## Compile using rebar if it exists else using make
compile:
	if which rebar > /dev/null; \
	then rebar compile; \
	else $(MAKE) $(MFLAGS) erlc_compile; \
	fi

## Compile using erlc
erlc_compile: $(addprefix $(BINDIR)/, $(EBINS))

install:
	if [ "$$ERL_LIBS" != "" ]; \
	then mkdir -p $(INSTALLDIR)/$(BINDIR) ; \
	     cp -pPR $(BINDIR) $(INSTALLDIR); \
	     cp -pPR $(EMACSDIR) $(INSTALLDIR); \
	     cp -pPR $(INCDIR) $(INSTALLDIR); \
	else exit 1; \
	fi

docs:

clean:
	if which rebar > /dev/null; \
	then rebar clean; \
	else rm -rf $(BINDIR)/*.beam; \
	fi
	rm -rf erl_crash.dump
