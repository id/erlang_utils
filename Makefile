.PHONY: all

ERL_SOURCES := $(wildcard src/*.erl)
ERL_OBJECTS := $(addprefix ebin/, $(notdir $(ERL_SOURCES:%.erl=%.beam)))

all: $(ERL_OBJECTS)

ebin/%.beam: src/%.erl
	echo $<
	erlc -o ebin $<

