-module(discogs).

-export([get_masters/2, acc_master_releases/2]).

-export([format_search_url/2]).

get_masters(Record, Token) ->
    Method = get,
    Url = format_search_url(Record, Token),
    ReqHeaders = [{<<"User-Agent">>, <<"discogswatcher/1.0">>}],
    Payload = <<>>,
    Options = [{pool, default}],
    {ok, _, _, Ref} = hackney:request(Method, Url, ReqHeaders, Payload, Options),
    hackney:body(Ref).



acc_master_releases(Releases, Token) ->
    ok.


format_search_url(Record, Token) ->
    {Artist, Title, Format} = Record,
    BaseUrl = "https://api.discogs.com/database/search",
    lists:flatten(io_lib:format("~s?artist=~s&title=~s&format=~s&token=~s", [BaseUrl, Artist, Title, Format, Token])).
