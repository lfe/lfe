# Makefile for LFE
# This simple Makefile uses rebar (in Unix) or rebar.cmd (in Windows)
# to compile/clean if it exists, else does it explicitly.

BINDIR = bin
EBINDIR = ebin
SRCDIR = src
CSRCDIR = c_src
INCDIR = include
DOCDIR = doc
EMACSDIR = emacs

VPATH = $(SRCDIR)

ERLCFLAGS = -W1
ERLC = erlc

EXPM=$(BINDIR)/expm
LIB=lfe

# To run erl as bash
FINISH=-run init stop -noshell

# Scripts to be evaluated
MAPS_MK = 'Has=erl_internal:bif(is_map,1), \
	HasMaps=if Has -> "-DHAS_MAPS=true\n" ; true -> "\n" end, \
	file:write_file("maps.mk", "HAS_MAPS = " ++ HasMaps)' \
	$(FINISH)

GET_VERSION = '{ok,[App]}=file:consult("src/$(LIB).app.src"), \
	V=proplists:get_value(vsn,element(3,App)), \
	io:format("~p~n",[V])' \
	$(FINISH)


## The .erl, .xrl, .yrl and .beam files
ESRCS = $(notdir $(wildcard $(SRCDIR)/*.erl))
XSRCS = $(notdir $(wildcard $(SRCDIR)/*.xrl))
YSRCS = $(notdir $(wildcard $(SRCDIR)/*.yrl))
EBINS = $(ESRCS:.erl=.beam) $(XSRCS:.xrl=.beam) $(YSRCS:.yrl=.beam)

CSRCS = $(notdir $(wildcard $(CSRCDIR)/*.c))
BINS = $(CSRCS:.c=)

## Where we install links to the LFE binaries.
DESTBINDIR = $(PREFIX)$(shell dirname `which erl` 2> /dev/null || echo "/usr/local/bin" )

.SUFFIXES: .erl .beam

$(BINDIR)/%: $(CSRCDIR)/%.c
	cc -o $@ $<

$(EBINDIR)/%.beam: $(SRCDIR)/%.erl
	$(ERLC) -I $(INCDIR) -o $(EBINDIR) $(HAS_MAPS) $(ERLCFLAGS) $<

%.erl: %.xrl
	$(ERLC) -o $(SRCDIR) $<

%.erl: %.yrl
	$(ERLC) -o $(SRCDIR) $<

all: compile docs

.PHONY: compile erlc_compile install docs clean

## Compile using rebar if it exists else using make
compile: maps.mk
	if which rebar.cmd > /dev/null; \
	then rebar.cmd compile; \
	elif which rebar > /dev/null; \
	then rebar compile; \
	else $(MAKE) $(MFLAGS) erlc_compile; \
	fi

## Compile using erlc
erlc_compile: $(addprefix $(EBINDIR)/, $(EBINS)) $(addprefix $(BINDIR)/, $(BINS))

maps.mk:
	erl -eval $(MAPS_MK)

-include maps.mk

install:
	ln -s `pwd`/bin/lfe $(DESTBINDIR)
	ln -s `pwd`/bin/lfec $(DESTBINDIR)
	ln -s `pwd`/bin/lfescript $(DESTBINDIR)

docs:

clean:
	if which rebar.cmd > /dev/null; \
	then rebar.cmd clean; \
	elif which rebar > /dev/null; \
	then rebar clean; \
	else rm -rf $(EBINDIR)/*.beam; \
	fi
	rm maps.mk
	rm -rf erl_crash.dump

echo:
	@ echo $(ESRCS)
	@ echo $(XSRCS)
	@ echo $(YSRCS)
	@ echo $(EBINS)

$(EXPM): $(BINDIR)
	curl -o $(EXPM) http://expm.co/__download__/expm
	chmod +x $(EXPM)

get-deps: $(EXPM)
	if which rebar.cmd > /dev/null; \
	then rebar.cmd get-deps; \
	elif which rebar > /dev/null; \
	then rebar get-deps; \
	fi

get-version:
	@echo
	@echo "Getting version info ..."
	@echo
	@echo -n app.src: ''
	@erl -eval $(GET_VERSION)
	@echo -n package.exs: ''
	@grep version package.exs | awk '{print $$2}'| sed -e 's/,//g'

upload: get-deps get-version
	@echo
	@echo "Continue with upload? "
	@read
	$(EXPM) publish
