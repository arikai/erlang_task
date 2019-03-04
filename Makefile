all: test_db test_map_reduce

test_db:
	cd db && rebar3 eunit

test_map_reduce:
	cd map_reduce && rebar3 eunit

.PHONY: all test_db test_map_reduce
