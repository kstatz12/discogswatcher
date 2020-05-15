-module(config).

-export([parse/1]).

parse(FilePath) ->
    Documents = get_documents(FilePath),
    [Document | _] = Documents,
    [Mappings | _] = Document,
    {_, Mapping} = Mappings,
    extract(Mapping).

get_documents(FilePath) ->
    yamerl_constr:file(FilePath, [{schema, json}]).

extract(L) ->
    lists:map(fun(X) -> to_tuple(X) end, L).

to_tuple(PropList) ->
    Title = proplists:get_value("Title", PropList),
    Artist = proplists:get_value("Artist", PropList),
    MaxPrice = proplists:get_value("MaxPrice", PropList),
    Currency = proplists:get_value("Currency", PropList),
    {Title, Artist, MaxPrice, Currency}.
