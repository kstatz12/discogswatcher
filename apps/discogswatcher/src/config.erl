-module(config).
-include_lib("eunit/include/eunit.hrl").

-export([get_discogs_token/2, get_records/2]).

get_discogs_token(Parser, FilePath) ->
    Document = parse(Parser, FilePath),
    Values = proplists:get_value("Config", Document),
    proplists:get_value("DiscogsToken", Values).

get_records(Parser, FilePath) ->
    Document = parse(Parser, FilePath),
    Values = proplists:get_value("Records", Document),
    lists:map(fun(X) -> to_records_tuple(X) end, Values).

parse(Parser, FilePath) ->
    Documents = Parser(FilePath),
    [Document | _] = Documents,
    Document.

to_records_tuple(PropList) ->
    Title = proplists:get_value("Title", PropList),
    Artist = proplists:get_value("Artist", PropList),
    Format = proplists:get_value("Format", PropList),
    {Title, Artist, Format}.

%% Tests %%
get_records_test() ->
    [{"Dopesmoker","Sleep","LP"}] = get_records(fun test_parser/1, "").

get_discogs_token_test() ->
    "cqcypeeIsRZugzdXdkDthLGASGlObmCoxGRMANRZ" = get_discogs_token(fun test_parser/1, "").

test_parser(_FilePath) ->
    [[{"Config",
       [{"DiscogsToken",
         "cqcypeeIsRZugzdXdkDthLGASGlObmCoxGRMANRZ"}]},
      {"Records",
       [[{"Artist","Sleep"},
         {"Title","Dopesmoker"},
         {"Format","LP"}]]}]].


