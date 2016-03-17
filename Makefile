# Makefile for LFE

BINDIR = bin
EBINDIR = ebin
SRCDIR = src
CSRCDIR = c_src
LSRCDIR = src
INCDIR = include
DOCDIR = doc
EMACSDIR = emacs

VPATH = $(SRCDIR)

ERLCFLAGS = -W1
ERLC = erlc

LFECFLAGS = -pa ../lfe
LFEC = $(BINDIR)/lfe $(BINDIR)/lfec
APP_SRC = lfe.app

LIB=lfe

# To run erl as bash
FINISH=-run init stop -noshell

# Scripts to be evaluated

GET_VERSION = '{ok,[App]}=file:consult("src/$(LIB).app.src"), \
	V=proplists:get_value(vsn,element(3,App)), \
	io:format("~p~n",[V])' \
	$(FINISH)


## The .erl, .xrl, .yrl and .beam files
ESRCS = $(notdir $(wildcard $(SRCDIR)/*.erl))
XSRCS = $(notdir $(wildcard $(SRCDIR)/*.xrl))
YSRCS = $(notdir $(wildcard $(SRCDIR)/*.yrl))
LSRCS = $(notdir $(wildcard $(LSRCDIR)/*.lfe))
EBINS = $(ESRCS:.erl=.beam) $(XSRCS:.xrl=.beam) $(YSRCS:.yrl=.beam)
LBINS = $(LSRCS:.lfe=.beam)

CSRCS = $(notdir $(wildcard $(CSRCDIR)/*.c))
BINS = $(CSRCS:.c=)

EMACSRCS = $(notdir $(wildcard $(EMACSDIR)/*.el))
ELCS = $(EMACSRCS:.el=.elc)

## Where we install links to the LFE binaries.
DESTBINDIR = $(PREFIX)$(shell dirname `which erl` 2> /dev/null || echo "/usr/local/bin" )

.SUFFIXES: .erl .beam

$(BINDIR)/%: $(CSRCDIR)/%.c
	cc -o $@ $<

$(EBINDIR)/%.beam: $(SRCDIR)/%.erl
	@mkdir -p $(EBINDIR)
	$(ERLC) -I $(INCDIR) -o $(EBINDIR) $(MAPS_OPTS) $(ERLCFLAGS) $<

%.erl: %.xrl
	$(ERLC) -o $(SRCDIR) $<

%.erl: %.yrl
	$(ERLC) -o $(SRCDIR) $<

$(EBINDIR)/%.beam: $(LSRCDIR)/%.lfe
	$(LFEC) -I $(INCDIR) -o $(EBINDIR) $(LFECFLAGS) $<

all: compile docs

.PHONY: compile erlc-compile lfec-compile erlc-lfec emacs install docs clean docker-build docker-push docker

compile: maps_opts.mk
	$(MAKE) $(MFLAGS) erlc-lfec

## Compile using erlc
erlc-compile: $(addprefix $(EBINDIR)/, $(EBINS)) $(addprefix $(BINDIR)/, $(BINS))

## Compile using lfec
lfec-compile: $(addprefix $(EBINDIR)/, $(LBINS))

$(APP_SRC):
	cp src/$(APP_SRC).src $(EBINDIR)/$(APP_SRC)

erlc-lfec: erlc-compile lfec-compile $(APP_SRC)

emacs:
	cd $(EMACSDIR) ; \
	emacs -L . -batch -f batch-byte-compile inferior-lfe.el lfe-mode.el lfe-indent.el

maps_opts.mk:
	escript get_maps_opts.escript

-include maps_opts.mk

install:
	ln -sf `pwd`/bin/lfe $(DESTBINDIR)
	ln -sf `pwd`/bin/lfec $(DESTBINDIR)
	ln -sf `pwd`/bin/lfescript $(DESTBINDIR)

docs:

clean:
	rm -rf $(EBINDIR)/*.beam erl_crash.dump maps_opts.mk

echo:
	@ echo $(ESRCS)
	@ echo $(XSRCS)
	@ echo $(YSRCS)
	@ echo $(EBINS)

get-version:
	@echo
	@echo "Getting version info ..."
	@echo
	@echo -n app.src: ''
	@erl -eval $(GET_VERSION)

# Target to regenerate the src/lfe_parse.erl file from its original
# src/lfe_parse.spell1 definition.  You will need to have spell1
# installed somewhere in your $ERL_LIBS path.
regenerate-parser:
	erl -noshell -eval 'spell1:file("src/lfe_parse", [report,verbose,{outdir,"./src/"},{includefile,code:lib_dir(spell1,include) ++ "/spell1inc.hrl"}]), init:stop().'

docker-build:
	docker build -t lfex/lfe:latest .

docker-run:
	docker run -i -t lfex/lfe:latest lfe

docker-push:
	docker push lfex/lfe:latest

docker: docker-build docker-push

travis:
	@echo "Building for Travis CI ..."
	@make

