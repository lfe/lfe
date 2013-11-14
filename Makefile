# Makefile for LFE
# This simple Makefile uses rebar to compile/clean if it
# exists, else does it explicitly.

BINDIR = ebin
SRCDIR = src
INCDIR = include
DOCDIR = doc
EMACSDIR = emacs

VPATH = $(SRCDIR)

ERLCFLAGS = -W1
ERLC = erlc

## The .erl, .xrl, .yrl and .beam files
ESRCS = $(notdir $(wildcard $(SRCDIR)/*.erl))
XSRCS = $(notdir $(wildcard $(SRCDIR)/*.xrl))
YSRCS = $(notdir $(wildcard $(SRCDIR)/*.yrl))
EBINS = $(ESRCS:.erl=.beam) $(XSRCS:.xrl=.beam) $(YSRCS:.yrl=.beam)

## Where we install LFE, in the ERL_LIBS directory.
INSTALLDIR = $(ERL_LIBS)/lfe-$(shell cat VERSION)

.SUFFIXES: .erl .beam

$(BINDIR)/%.beam: $(SRCDIR)/%.erl
	$(ERLC) -I $(INCDIR) -o $(BINDIR) $(ERLCFLAGS) $<

%.erl: %.xrl
	$(ERLC) -o $(SRCDIR) $<

%.erl: %.yrl
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
	then mkdir -p $(INSTALLDIR) ; \
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

echo:
	@ echo $(ESRCS)
	@ echo $(XSRCS)
	@ echo $(YSRCS)
	@ echo $(EBINS)
