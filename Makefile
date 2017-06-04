# Copyright (c) 2016 Robert Virding
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
#  limitations under the License.

# Makefile for LFE

BINDIR = bin
EBINDIR = ebin
SRCDIR = src
CSRCDIR = c_src
LSRCDIR = src
INCDIR = include
EMACSDIR = emacs
PREFIX ?= /usr/local
INSTALL = install
INSTALL_DIR = $(INSTALL) -m755 -d
INSTALL_DATA = $(INSTALL) -m644
INSTALL_BIN = $(INSTALL) -m755
DESTLIBDIR := $(PREFIX)/lib/lfe
DESTEBINDIR := $(DESTLIBDIR)/$(EBINDIR)
DESTBINDIR := $(DESTLIBDIR)/$(BINDIR)

VPATH = $(SRCDIR)

MKDIR_P = mkdir -p
MANDB = $(shell which mandb)

ERLCFLAGS = -W1
ERLC = erlc

LFECFLAGS = -pa ../lfe
LFEC = $(BINDIR)/lfe $(BINDIR)/lfec
APP_DEF = lfe.app

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

.SUFFIXES: .erl .beam

$(BINDIR)/%: $(CSRCDIR)/%.c
	cc -o $@ $<

$(EBINDIR)/%.beam: $(SRCDIR)/%.erl
	@$(MKDIR_P) $(EBINDIR)
	$(ERLC) -I $(INCDIR) -o $(EBINDIR) $(COMP_OPTS) $(ERLCFLAGS) $<

%.erl: %.xrl
	$(ERLC) -o $(SRCDIR) $<

%.erl: %.yrl
	$(ERLC) -o $(SRCDIR) $<

$(EBINDIR)/%.beam: $(LSRCDIR)/%.lfe
	$(LFEC) -I $(INCDIR) -o $(EBINDIR) $(LFECFLAGS) $<

all: compile

.PHONY: compile erlc-compile lfec-compile erlc-lfec emacs install docs clean docker-build docker-push docker update-mandb

compile: comp_opts.mk
	$(MAKE) $(MFLAGS) erlc-lfec

## Compile Erlang files using erlc
erlc-compile: $(addprefix $(EBINDIR)/, $(EBINS)) $(addprefix $(BINDIR)/, $(BINS))

## Compile LFE files using lfec
lfec-compile: $(addprefix $(EBINDIR)/, $(LBINS))

$(addprefix $(EBINDIR)/, $(LBINS)): $(addprefix $(EBINDIR)/, $(EBINS))

$(EBINDIR)/$(APP_DEF): $(SRCDIR)/$(APP_DEF).src
	cp $(SRCDIR)/$(APP_DEF).src $(EBINDIR)/$(APP_DEF)

## Compile Erlang files using erlc and LFE files using lfec
erlc-lfec: erlc-compile $(EBINDIR)/$(APP_DEF) lfec-compile

emacs:
	cd $(EMACSDIR) ; \
	emacs -L . -batch -f batch-byte-compile inferior-lfe.el lfe-mode.el lfe-indent.el

comp_opts.mk:
	escript get_comp_opts.escript

-include comp_opts.mk

$(BINDIR)/lfe%:
	$(INSTALL_BIN) $@ $(DESTBINDIR)

install: compile install-man
	rm -Rf $(DESTEBINDIR)
	$(INSTALL_DIR) $(DESTEBINDIR)
	$(INSTALL_DATA) $(EBINDIR)/$(APP_DEF) $(DESTEBINDIR)
	$(INSTALL_DATA) $(addprefix $(EBINDIR)/, $(EBINS)) $(DESTEBINDIR)
	$(INSTALL_DATA) $(addprefix $(EBINDIR)/, $(LBINS)) $(DESTEBINDIR)
	$(INSTALL_DIR) $(DESTBINDIR)
	$(MAKE) $(BINDIR)/lfe $(BINDIR)/lfec $(BINDIR)/lfedoc $(BINDIR)/lfescript
	ln -sf $(DESTBINDIR)/* $(PREFIX)/bin/

clean:
	rm -rf $(EBINDIR)/*.beam erl_crash.dump comp_opts.mk

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

# Targets for generating docs and man pages
DOCDIR = doc
DOCSRC = $(DOCDIR)/src
MANDIR = $(DOCDIR)/man
PDFDIR = $(DOCDIR)/pdf
EPUBDIR = $(DOCDIR)/epub
MANINSTDIR ?= $(PREFIX)/share/man

MAN1_SRCS = $(notdir $(wildcard $(DOCSRC)/*1.md))
MAN1S = $(MAN1_SRCS:.1.md=.1)
TXT1S = $(MAN1_SRCS:.1.md=.txt)
PDF1S = $(MAN1_SRCS:.1.md=.pdf)
EPUB1S = $(MAN1_SRCS:.1.md=.epub)
MAN3_SRCS = $(notdir $(wildcard $(DOCSRC)/*3.md))
MAN3S = $(MAN3_SRCS:.3.md=.3)
TXT3S = $(MAN3_SRCS:.3.md=.txt)
PDF3S = $(MAN3_SRCS:.3.md=.pdf)
EPUB3S = $(MAN3_SRCS:.3.md=.epub)
MAN7_SRCS = $(notdir $(wildcard $(DOCSRC)/*7.md))
MAN7S = $(MAN7_SRCS:.7.md=.7)
TXT7S = $(MAN7_SRCS:.7.md=.txt)
PDF7S = $(MAN7_SRCS:.7.md=.pdf)
EPUB7S = $(MAN7_SRCS:.7.md=.epub)

# Just generate the docs that are tracked in git
docs: docs-txt

# Generate all docs, even those not tracked in git
docs-all: docs docs-epub docs-pdf

docs-man: \
	$(addprefix $(MANDIR)/, $(MAN1S)) \
	$(addprefix $(MANDIR)/, $(MAN3S)) \
	$(addprefix $(MANDIR)/, $(MAN7S))


$(MANDIR)/%.1: $(DOCSRC)/%.1.md
	pandoc -f markdown -s -t man -o $@ $<

$(MANDIR)/%.3: $(DOCSRC)/%.3.md
	pandoc -f markdown -s -t man -o $@ $<

$(MANDIR)/%.7: $(DOCSRC)/%.7.md
	pandoc -f markdown -s -t man -o $@ $<

clean-docs:
	rm -f $(DOCDIR)/*.txt $(MANDIR)/*.[0-9] $(PDFDIR)/*.pdf $(EPUBDIR)/*.epub

docs-txt: docs-man \
	$(addprefix $(DOCDIR)/, $(TXT1S)) \
	$(addprefix $(DOCDIR)/, $(TXT3S)) \
	$(addprefix $(DOCDIR)/, $(TXT7S))
	@if [ -f $(DOCDIR)/lfe_guide.txt ]; then \
		cp $(DOCDIR)/lfe_guide.txt $(DOCDIR)/user_guide.txt ; \
	fi

$(DOCDIR)/%.txt: export GROFF_NO_SGR=1

$(DOCDIR)/%.txt: $(MANDIR)/%.1
	groff -t -e -mandoc -Tutf8 -Kutf8 $< | col -bx > $@

$(DOCDIR)/%.txt: $(MANDIR)/%.3
	groff -t -e -mandoc -Tutf8 -Kutf8 $< | col -bx > $@

$(DOCDIR)/%.txt: $(MANDIR)/%.7
	groff -t -e -mandoc -Tutf8 -Kutf8 $< | col -bx > $@

$(PDFDIR):
	@$(MKDIR_P) $(PDFDIR)

docs-pdf: $(PDFDIR) \
	$(addprefix $(PDFDIR)/, $(PDF1S)) \
	$(addprefix $(PDFDIR)/, $(PDF3S)) \
	$(addprefix $(PDFDIR)/, $(PDF7S))

$(PDFDIR)/%.pdf: $(DOCSRC)/%.1.md
	pandoc -f markdown --latex-engine=xelatex -o $@ $<

$(PDFDIR)/%.pdf: $(DOCSRC)/%.3.md
	pandoc -f markdown --latex-engine=xelatex -o $@ $<

$(PDFDIR)/%.pdf: $(DOCSRC)/%.7.md
	pandoc -f markdown --latex-engine=xelatex -o $@ $<

$(EPUBDIR):
	@$(MKDIR_P) $(EPUBDIR)

docs-epub: $(EPUBDIR) \
	$(addprefix $(EPUBDIR)/, $(EPUB1S)) \
	$(addprefix $(EPUBDIR)/, $(EPUB3S)) \
	$(addprefix $(EPUBDIR)/, $(EPUB7S))

$(EPUBDIR)/%.epub: $(DOCSRC)/%.1.md
	pandoc -f markdown -t epub -o $@ $<

$(EPUBDIR)/%.epub: $(DOCSRC)/%.3.md
	pandoc -f markdown -t epub -o $@ $<

$(EPUBDIR)/%.epub: $(DOCSRC)/%.7.md
	pandoc -f markdown -t epub -o $@ $<

$(MANINSTDIR)/man%:
	@$(MKDIR_P) -p $@

ifeq (,$(findstring mandb,$(MANDB)))
install-man: $(MANINSTDIR)/man1 $(MANINSTDIR)/man3 $(MANINSTDIR)/man7
else
install-man: $(MANINSTDIR)/man1 $(MANINSTDIR)/man3 $(MANINSTDIR)/man7 update-mandb
endif
	$(INSTALL_DATA) $(MANDIR)/*.1 $(MANINSTDIR)/man1/
	$(INSTALL_DATA) $(MANDIR)/*.3 $(MANINSTDIR)/man3/
	$(INSTALL_DATA) $(MANDIR)/*.7 $(MANINSTDIR)/man7/

update-mandb:
	@echo "Updating man page database ..."
	$(MANDB) $(MANINSTDIR)

# Targets for working with Docker
docker-build:
	docker build -t lfex/lfe:latest .
	docker build -t lfex/lfe-docs:latest ./doc

docker-push:
	docker push lfex/lfe:latest
	docker push lfex/lfe-docs:latest

docker: docker-build docker-push

docker-run:
	docker run -i -t lfex/lfe:latest lfe

docker-docs:
	docker run -v `pwd`/doc:/docs -t lfex/lfe-docs:latest
	sudo chown -R $(USER):$(USER) doc/*

docker-docs-bash:
	docker run -i -v `pwd`/doc:/docs -t lfex/lfe-docs:latest bash

travis:
	@rebar3 ct
	@rebar3 eunit -m clj-tests,prop_lfe_doc
