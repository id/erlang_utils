.PHONY: all

all: ebin/user_default.beam

ebin/user_default.beam: src/user_default.erl
	@erlc -o ebin src/user_default.erl

