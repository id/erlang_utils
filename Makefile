.PHONY: all

all: ebin/my_dbg.beam

ebin/my_dbg.beam: src/my_dbg.erl
	@erlc -o ebin src/my_dbg.erl

