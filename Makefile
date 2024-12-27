# Copyright (c) 2016-2020 Robert Virding
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

# Makefile for spell1

OS_NAME := $(shell uname -s | tr '[:upper:]' '[:lower:]')

EBINDIR = ebin
SRCDIR = src
INCDIR = include

VPATH = $(SRCDIR)

ERLCFLAGS = -W1
ERLC ?= erlc

LFECFLAGS = +debug-info
LFEC = lfec

APP_DEF = spell1.app

LIB = spell1

# To run erl as bash
FINISH=-run init stop -noshell

# Scripts to be evaluated

GET_VERSION = '{ok,[App]}=file:consult("src/$(LIB).app.src"), \
	V=proplists:get_value(vsn,element(3,App)), \
	io:format("~p~n",[V])' \
	$(FINISH)


## The .erl, .xrl, .yrl and .beam files
ESRCS = $(notdir $(wildcard $(SRCDIR)/*.erl))
LSRCS = $(notdir $(wildcard $(SRCDIR)/*.lfe))
EBINS = $(ESRCS:.erl=.beam)
LBINS = $(LSRCS:.lfe=.beam)

.SUFFIXES: .erl .lfe .beam

$(EBINDIR)/%.beam: $(SRCDIR)/%.erl
	$(ERLC) -I $(INCDIR) -o $(EBINDIR) $(COMP_OPTS) $(ERLCFLAGS) $<

$(EBINDIR)/%.beam: $(SRCDIR)/%.lfe
	$(LFEC) -I $(INCDIR) -o $(EBINDIR) $(LFECFLAGS) $<

all: compile

.PHONY: compile erlc-lfec erlc-compile lfec-compile rebar-compile docs clean clean-all

compile: comp_opts.mk
	$(MAKE) $(MFLAGS) erlc-lfec;

## Compile Erlang files using erlc and LFE files using lfec
erlc-lfec: erlc-compile $(EBINDIR)/$(APP_DEF) lfec-compile

## Compile Erlang files using erlc
erlc-compile: $(addprefix $(EBINDIR)/, $(EBINS)) $(addprefix $(BINDIR)/, $(BINS))

## Compile LFE files using lfec
lfec-compile: $(addprefix $(EBINDIR)/, $(LBINS))

## Compile with rebar 
rebar-compile:
	if which rebar3.cmd > /dev/null; \
	then rebar3.cmd compile; \
	elif which rebar3 > /dev/null; \
	then rebar3 compile; \
	fi

comp_opts.mk:
	escript get_comp_opts.escript

-include comp_opts.mk

# Generate the docs.
docs:

clean-all: clean
	rm -rf _build

clean:
	rm -rf $(EBINDIR)/*.beam erl_crash.dump comp_opts.mk

info:
	@ echo $(ESRCS)
	@ echo $(LSRCS)
	@ echo $(EBINS)

get-version:
	@echo
	@echo "Getting version info ..."
	@echo
	@echo -n app.src: ''
	@erl -eval $(GET_VERSION)
