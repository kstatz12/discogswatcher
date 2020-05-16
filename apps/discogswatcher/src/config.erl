-module(config).

-export([get_config/1, get_records/1]).

get_config(FilePath) ->
    Document = parse(FilePath),
    Values = proplists:get_value("Config", Document),
    to_config_tuple(Values).

get_records(FilePath) ->
    Document = parse(FilePath),
    Values = proplists:get_value("Records", Document),
    lists:map(fun(X) -> to_records_tuple(X) end, Values).

parse(FilePath) ->
    Documents = get_documents(FilePath),
    [Document | _] = Documents,
    Document.

get_documents(FilePath) ->
    yamerl_constr:file(FilePath, [{schema, json}]).

to_records_tuple(PropList) ->
    Title = proplists:get_value("Title", PropList),
    Artist = proplists:get_value("Artist", PropList),
    Format = proplists:get_value("Format", PropList),
    {Title, Artist, Format}.

to_config_tuple(PropList) ->
    Interval = proplists:get_value("DiscogsToken", PropList),
    {Interval}.

%% Tests 
