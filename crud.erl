db:new(DbName) - > ok

      DbName = string()

db:create(Record, DbName) - > {ok, Record} | {error, Reason}

      Record = {Key, UserName, City}

            Key = integer() (must be unique)

            UserName = string()

            City = string()

      Reason = term() %создание должно завершиться с ошибкой, если Key занят.

db:read(Key, DbName) - > {ok, Record} | {error, Reason}

      % чтение должно завершиться с ошибкой, если Key не существует в базе.

db:update(Record, DbName) - > {ok, Record} | {error, Reason}

      % обновление записи должно завершиться с ошибкой, если Key не существует в базе.

db:delete(Key, DbName) - > ok | {error, Reason}

      % удаление записи должно завершиться с ошибкой, если Key не существует в базе.
