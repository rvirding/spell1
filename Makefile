# Makefile for spell1
# This simple Makefile uses rebar (in Unix) or rebar.cmd (in Windows)
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

.PHONY: compile erlc_compile install docs clean

## Compile using rebar if it exists else using make
compile:
	if which rebar.cmd > /dev/null; \
	then rebar.cmd compile; \
	elif which rebar > /dev/null; \
	then rebar compile; \
	else $(MAKE) $(MFLAGS) erlc_compile; \
	fi

## Compile using erlc
erlc_compile: $(addprefix $(EBINDIR)/, $(EBINS)) $(addprefix $(BINDIR)/, $(BINS))

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
	@ echo $(LSRCS)
	@ echo $(EBINS)
