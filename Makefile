# Makefile for LFE
# This simple Makefile uses rebar to compile/clean if it
# exists, else does it explicitly.

EBINDIR = ebin
SRCDIR = src
INCDIR = include
DOCDIR = doc
EMACSDIR = emacs

VPATH = $(SRCDIR)

ERLCFLAGS = -W0 +debug_info
ERLC = erlc -I $(INCDIR) -o $(EBINDIR) $(ERLCFLAGS)

## The .erl and .beam files
SRCS = $(notdir $(wildcard $(SRCDIR)/*.erl))
EBINS = $(SRCS:.erl=.beam)

## Where we install LFE, in the ERL_LIBS directory.
INSTALLDIR = $(ERL_LIBS)/lfe

all: compile docs

compile:
	if which -s rebar; \
	then rebar compile; \
	else $(ERLC) $(addprefix $(SRCDIR)/, $(SRCS)); \
	fi

install:
	if [ "$$ERL_LIBS" != "" ]; \
	then mkdir -p $(INSTALLDIR)/$(EBINDIR) ; \
	     cp -pPR $(EBINDIR) $(INSTALLDIR); \
	     cp -pPR $(EMACSDIR) $(INSTALLDIR); \
	else exit 1; \
	fi

docs:

clean:
	if which -s rebar; \
	then rebar clean; \
	else rm -rf $(EBINDIR)/*.beam; \
	fi
	rm -rf erl_crash.dump 
