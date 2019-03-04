-module(map_reduce).
-export([
         start/1
        ]).

%% Задача2 (факультативно). Имеются несколько текстовых файлов:
%% data1.txt, data2.txt etc. Нужно выбрать все слова, которые содержаться
%% в этих файлах, и посчитать количество вхождений каждого слова.

%% Вам нужно реализовать функцию map_reduce:start/1, которая получает на
%% старте список файлов, а на выходе отдает карту, где ключи -- все
%% слова, встречающиеся в этих файлах, а значения -- количество вхождений
%% для каждого слова. Смотрите map_reduce_test.

%% Для каждого файла нужно стартовать отдельный Map-поток, который
%% прочитает данные из файла и посчитает слова в нем. Тут нужно также
%% обработать ситуацию, если заданный файл не удалось прочитать. Далее,
%% нужен поток Reduce, который соберет данные из всех Map-потоков,
%% суммирует их, и вернет результат.

%% Примечания:
%% 1. При неудачном чтении файла - пишем об этом в stderr

start(Filenames) -> start(Filenames, 0).

start([], SlaveNum) -> reduce(SlaveNum);
start([Filename | Rest], SlaveNum) when is_list(Filename) ->
    Master = self(),
    spawn(fun() -> parse_file(Master, Filename) end),
    start(Rest, SlaveNum+1).

reduce(SlaveNum) -> reduce(maps:new(), SlaveNum).

map_sum_merge(Map1, Map2) ->
    lists:foldl(
      fun({Key, Value}, MapAcc) ->
              maps:update_with(Key, fun(OldVal) -> OldVal + Value end, Value, MapAcc) end,
      Map1,
      maps:to_list(Map2)).

reduce(MapAcc, SlavesLeft) ->
    case SlavesLeft of
        0 -> MapAcc;
        _ ->
            receive
                {ok, _Filename, Map} ->
                    reduce( map_sum_merge( Map, MapAcc ), SlavesLeft-1);
                {error, Filename, Error} ->
                    io:format(standard_error, "Error parsing file \"~s\": ~p\n", [Filename, Error]),
                    reduce(MapAcc, SlavesLeft - 1)
            end
    end.

parse_file(Master, Filename) when is_list(Filename) ->
    Master ! case file:open(Filename, [read, read_ahead, {encoding, utf8} ]) of
                 {ok, File} -> {ok, Filename, parse(File)};
                 {error, Reason} -> {error, Filename, Reason}
             end.

parse(File) -> parse(File, maps:new()).

parse(File, Map) ->
    case io:get_line(File, '') of
        eof -> Map;
        {error, Reason} -> {error, Reason};
        Line ->
            parse(File, lists:foldl(
                          fun(Word, M) -> maps:update_with(Word, fun(V) -> V+1 end, 1, M) end,
                          Map,
                          string:lexemes(Line, " ,.;!?\n")
                         ))
    end.
