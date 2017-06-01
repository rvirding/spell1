# Makefile for spell1
# This simple Makefile uses rebar3 (in Unix) or rebar3.cmd (in Windows)
# to compile/clean if it exists, else does it explicitly.

EBINDIR = ebin
SRCDIR = src
INCDIR = include

VPATH = $(SRCDIR)

ERLCFLAGS = -W1
ERLC = erlc

LFEC = lfec

## The .erl, .xrl, .yrl and .beam files
ESRCS = $(notdir $(wildcard $(SRCDIR)/*.erl))
LSRCS = $(notdir $(wildcard $(SRCDIR)/*.lfe))
EBINS = $(ESRCS:.erl=.beam) $(LSRCS:.lfe=.beam)

.SUFFIXES: .erl .lfe .beam

$(EBINDIR)/%.beam: $(SRCDIR)/%.erl
	$(ERLC) -I $(INCDIR) -o $(EBINDIR) $(ERLCFLAGS) $<

$(EBINDIR)/%.beam: $(SRCDIR)/%.lfe
	$(LFEC) -I $(INCDIR) -o $(EBINDIR) $(ERLCFLAGS) $<

all: compile docs

.PHONY: compile erlc-compile install docs clean

## Compile using rebar3 if it exists else using make
compile:
	if which rebar3.cmd > /dev/null; \
	then rebar3.cmd compile; \
	elif which rebar3 > /dev/null; \
	then rebar3 compile; \
	else $(MAKE) $(MFLAGS) erlc-compile; \
	fi

## Compile using erlc
erlc-compile: $(addprefix $(EBINDIR)/, $(EBINS)) $(addprefix $(BINDIR)/, $(BINS))

docs:

clean:
	if which rebar3.cmd > /dev/null; \
	then rebar3.cmd clean; \
	elif which rebar3 > /dev/null; \
	then rebar3 clean; \
	else rm -rf $(EBINDIR)/*.beam; \
	fi
	rm -rf maps.mk
	rm -rf erl_crash.dump

echo:
	@ echo $(ESRCS)
	@ echo $(LSRCS)
	@ echo $(EBINS)
