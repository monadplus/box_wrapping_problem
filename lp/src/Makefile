################################### IGNORE #############################################

SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

ifeq ($(origin .RECIPEPREFIX), undefined)
  $(error This Make does not support .RECIPEPREFIX. Please use GNU Make 4.0 or later)
endif
.RECIPEPREFIX = >

############################### CONFIGURATION ###########################################

CCC = g++ -std=c++11

CCOPT = -m64 -O3 -fPIC -fno-strict-aliasing -fexceptions -DNDEBUG -DIL_STD

SYSTEM     = x86-64_linux
LIBFORMAT  = static_pic

# Please, change both routes!
CPLEXDIR      = /nix/store/zsrmkd42gcmsm5g1a9lycag5blbk32q4-home-manager-path/cplex
CONCERTDIR    = /home/arnau/MIRI/CPS/lp/cplex/concert

CPLEXBINDIR   = $(CPLEXDIR)/bin/$(BINDIST)
CPLEXLIBDIR   = $(CPLEXDIR)/lib/$(SYSTEM)/$(LIBFORMAT)
CONCERTLIBDIR = $(CONCERTDIR)/lib/$(SYSTEM)/$(LIBFORMAT)

CONCERTINCDIR = $(CONCERTDIR)/include
CPLEXINCDIR   = $(CPLEXDIR)/include
CCFLAGS = $(CCOPT) -I$(CPLEXINCDIR) -I$(CONCERTINCDIR)

CCLNFLAGS = -L$(CPLEXLIBDIR) -lilocplex -lcplex -L$(CONCERTLIBDIR) -lconcert -lm -pthread -ldl

################################# MAIN ########################################

all: p checker
.PHONY: all

p: box.cpp
> $(CCC) -c $< $(CCFLAGS)
> $(CCC) -o $@ $< $(CCFLAGS) $(CCLNFLAGS)

checker: checker.cc
> $(CXX) -c $< -O3
> $(CXX) -o $@ $<

clean:
> rm --force *.o p checker
.PHONY: clean
