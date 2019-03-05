%% ETS-Based DB API

-module(db).
-export([
         new/1,
         create/2,
         read/2,
         update/2,
         delete/2,
         delete_db/1
        ]).

%% Record format:
%% { Key, UserName, City }
%% Key      = integer() (must be unique)
%% UserName = string()
%% City     = string()

-type(key()        :: integer() ).
-type(username()   :: string()  ).
-type(city()       :: string()  ).
-type(db_name()    :: string()  ).
-type(err_msg()    :: atom() | term() ).
-type(status()     :: ok | error ).

-type(db_record()  :: { key(), username(), city() }).

-spec( new( DbName::db_name() ) -> ok ).
new(DbName) when is_list(DbName) ->
    DB = list_to_atom(DbName),
    ets:new(DB, [
                 set,
                 public,
                 {write_concurrency, true},
                 named_table
                ]),
    ok.

-spec( create(Record :: db_record(), DbName :: db_name() ) -> { ok, db_record() } | { error, err_msg() } ).
create(Record, DbName) when is_tuple(Record), is_list(DbName) ->
    DB = list_to_atom(DbName),
    try
        case ets:insert_new(DB, Record) of
            true -> { ok, Record };
            false -> { error, key_exists }
        end
    catch
        error:Error -> { error, Error }
    end.

-spec( read(Key :: key(), DbName :: db_name() ) -> { ok, db_record() } | { error, err_msg() } ).
read(Key, DbName) when is_integer(Key), is_list(DbName) ->
    DB = list_to_atom(DbName),
    try
        case ets:lookup(DB, Key) of
            [Record] -> {ok, Record};
            [] -> {error, key_not_found};
            _List -> {error, many_records_found}  % Impossible for set
        end
    catch
        error:Error -> { error, Error }
    end.

-spec( update(Record :: db_record(), DbName :: db_name() ) -> { ok, db_record() } | { error, err_msg() } ).
update(Record, DbName) ->
    DB = list_to_atom(DbName),
    try
        case ets:member(DB, element(1, Record)) of
            true ->
                case ets:insert(DB, Record) of
                    true -> {ok, Record}
                end;
            false -> {error, key_not_found}
        end
    catch
        error:Error -> {error, Error}
    end.

-spec( delete( Key :: key(), DbName :: db_name() ) -> true | { error, err_msg() } ).
delete(Key, DbName) ->
    DB = list_to_atom(DbName),
    try
        case ets:member(DB, Key) of
            true -> case ets:delete(DB, Key) of
                        true -> ok
                    end;
            false -> {error, key_not_found}
        end
    catch
        error:Error -> {error, Error}
    end.

delete_db(DbName) ->
    ets:delete(list_to_atom(DbName)).
