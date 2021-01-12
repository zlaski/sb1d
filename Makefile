##############################################################################
# Makefile  (Tab char: [	])                                               #
#                                                                            #
# Copyright (c) 2021 by Ziemowit Laski.  All rights reserved.                #
#                                                                            #
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND  WITHOUT  ANY  EXPRESSED  OR       #
# IMPLIED  WARRANTIES,  INCLUDING,  WITHOUT LIMITATION, THE IMPLIED          #
# WARRANTIES  OF  MERCHANTABILITY  AND  FITNESS  FOR  A  PARTICULAR          #
# PURPOSE.                                                                   #
#                                                                            #
##############################################################################

.PHONY: blddir

SOURCES = $(wildcard src/*.cpp)
HEADERS = $(wildcard src/*.h)

OBJECTS = $(SOURCES:src/%.cpp=bld/%.o)

CXX = g++

all: blddir bld/sb1d

bld/sb1d: $(OBJECTS)
	$(CXX) -o $@ $+

# No need to bring makedepend into the mix.  We shall simply assume that
# a change to ANY header will trigger a full recompile.
$(OBJECTS): $(HEADERS)

$(OBJECTS): bld/%.o: src/%.cpp
	$(CXX) -c -std=c++11 $(CXXFLAGS) $(CPPFLAGS) $< -o $@

blddir:
	mkdir -p bld

clean:
	rm -rf bld
