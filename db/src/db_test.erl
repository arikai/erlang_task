-module(db_test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

create_no_db_test() ->
    DB = "mydb",
    User = {1, "Bobby", "New York"},
    ?assertMatch({error, _}, db:create(User, DB)).

new_test() ->
    DB = "mydb",
    ?assertMatch(ok, db:new(DB)),
    db:delete_db(DB).

create_test() ->
    DB = "mydb",
    db:new(DB),
    User = {1, "Bobby", "New York"},
    ?assertMatch({ok, User}, db:create(User, DB)),
    db:delete_db(DB).

create_double_test() ->
    DB = "mydb",
    db:new(DB),
    User = {1, "Bobby", "New York"},
    db:create(User, DB),
    ?assertMatch({ error, key_exists }, db:create(User, DB)),
    db:delete_db(DB).

read_test() ->
    DB = "mydb",
    db:new(DB),
    User = {1, "Bobby", "New York"},
    db:create(User, DB),
    ?assertMatch({ok, User}, db:read(element(1, User), DB) ),
    db:delete_db(DB).

read_non_existant_test() ->
    DB = "mydb",
    db:new(DB),
    User = {1, "Bobby", "New York"},
    ?assertMatch({error, key_not_found}, db:read(element(1, User), DB) ),
    db:delete_db(DB).

update_test() ->
    DB = "mydb",
    db:new(DB),
    User = {1, "Bobby", "New York"},
    db:create(User, DB),
    UpdatedUser = setelement(3, User, "California"),
    ?assertMatch({ok, UpdatedUser}, db:update(UpdatedUser, DB)),
    ?assertMatch({ok, UpdatedUser}, db:read(element(1, UpdatedUser), DB)),
    db:delete_db(DB).

delete_non_existant_test() ->
    DB = "mydb",
    db:new(DB),
    ?assertMatch({ error, key_not_found }, db:delete(2, DB)), % Error
    db:delete_db(DB).

delete_test() ->
    DB = "mydb",
    db:new(DB),
    User = {2, "Ivan", "Moscow"},
    db:create(User, DB),
    ?assertEqual( ok, db:delete(2, DB)),
    ?assertMatch({ error, key_not_found }, db:read(2, DB)), % Error
    db:delete_db(DB).

-endif.
