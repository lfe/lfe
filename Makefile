# Makefile for LFE
# This simple Makefile uses rebar to compile/clean if it
# exists, else does it explicitly.

BINDIR = bin
EBINDIR = ebin
SRCDIR = src
INCDIR = include
DOCDIR = doc
EMACSDIR = emacs

VPATH = $(SRCDIR)

ERLCFLAGS = -W1
ERLC = erlc

EXPM=$(BINDIR)/expm
LIB=lfe

FINISH=-run init stop -noshell

## The .erl, .xrl, .yrl and .beam files
ESRCS = $(notdir $(wildcard $(SRCDIR)/*.erl))
XSRCS = $(notdir $(wildcard $(SRCDIR)/*.xrl))
YSRCS = $(notdir $(wildcard $(SRCDIR)/*.yrl))
EBINS = $(ESRCS:.erl=.beam) $(XSRCS:.xrl=.beam) $(YSRCS:.yrl=.beam)

## Where we install LFE, in the ERL_LIBS directory.
INSTALLDIR = $(ERL_LIBS)/lfe-$(shell cat VERSION)

.SUFFIXES: .erl .beam

$(EBINDIR)/%.beam: $(SRCDIR)/%.erl
	$(ERLC) -I $(INCDIR) -o $(EBINDIR) $(ERLCFLAGS) $<

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
erlc_compile: $(addprefix $(EBINDIR)/, $(EBINS))

install:
	if [ "$$ERL_LIBS" != "" ]; \
	then mkdir -p $(INSTALLDIR) ; \
	     cp -pPR $(EBINDIR) $(INSTALLDIR); \
	     cp -pPR $(EMACSDIR) $(INSTALLDIR); \
	     cp -pPR $(INCDIR) $(INSTALLDIR); \
	else exit 1; \
	fi

docs:

clean:
	if which rebar > /dev/null; \
	then rebar clean; \
	else rm -rf $(EBINDIR)/*.beam; \
	fi
	rm -rf erl_crash.dump

echo:
	@ echo $(ESRCS)
	@ echo $(XSRCS)
	@ echo $(YSRCS)
	@ echo $(EBINS)

$(BINDIR):
	mkdir -p $(BINDIR)

$(EXPM): $(BINDIR)
	curl -o $(EXPM) http://expm.co/__download__/expm
	chmod +x $(EXPM)

get-deps: $(EXPM)

get-version:
	@echo
	@echo -n app.src: ''
	@erl -eval 'io:format("~p~n", [ \
		proplists:get_value(vsn,element(3,element(2,hd(element(3, \
		erl_eval:exprs(element(2, erl_parse:parse_exprs(element(2, \
		erl_scan:string("Data = " ++ binary_to_list(element(2, \
		file:read_file("src/$(LIB).app.src"))))))), []))))))])' \
		$(FINISH)
	@echo -n package.exs: ''
	@grep version package.exs |awk '{print $$2}'|sed -e 's/,//g'

upload: get-version
	$(EXPM) publish
