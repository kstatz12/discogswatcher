-module(discogs).

-export([get_releases_for_record/2]).

-export([format_search_url/2]).
    
get_releases_for_record(Record, Token) ->
    Document = exec_request(format_search_url(Record, Token)),
    Items = proplists:get_value("results", Document),
    MasterUrls = lists:map(fun(X) -> proplists:get_value("master_url", X) end, Items),
    get_releases(MasterUrls, Token).
    
    
get_releases(MasterUrls, Token) when is_list(MasterUrls) ->
    UniqueMasterUrls = dedup(MasterUrls),
    lists:map(fun(X) -> acc_master_releases(X, Token) end, UniqueMasterUrls).
    % lists:map(fun(X) -> get_release(X, Token) end, MainReleaseIds)

    
    

acc_master_releases(Url, Token) ->
    Document = exec_request(format_master_url(Url, Token)),
    proplists:get_value("main_release", Document).

get_release(ReleaseId, Token) ->
    Url = format_release_url(ReleaseId,Token),
    Document = exec_request(Url),
    LowestPrice = proplists:get_value("lowest_price", Document),
    Notes = proplists:get_value("notes", Document),
    ReleaseYear = proplists:get_value("released_formatted", Document),
    NumForSale = proplists:get_value("num_for_sale", Document),
    {LowestPrice, NumForSale, Notes, ReleaseYear}.

exec_request(Url) ->
    {ok, _, _, Ref} = hackney:request(get, Url, [], <<>>, [{pool, default}]),
    {ok, Body} = hackney:body(Ref),
    Documents = yamerl_constr:string(Body),
    [Document | _] = Documents,
    Document.

format_master_url(Url, Token) ->
    Url = fmt_string("~?token=~s", [Url, Token]),
    list_to_binary(Url).

format_search_url(Record, Token) ->
    {Artist, Title, Format} = Record,
    BaseUrl = "https://api.discogs.com/database/search",
    Template = "~s?artist=~s&title=~s&format=~s&token=~s",
    Url = fmt_string(Template, [BaseUrl, Artist, Title, Format, Token]),
    list_to_binary(Url).

format_release_url(ReleaseId, Token) ->
    BaseUrl = "https://api.discogs.com/releases/~s?token=~s",
    fmt_string(BaseUrl, [ReleaseId, Token]).

dedup([]) -> [];
dedup([H | T]) -> [H | [X || X <- dedup(T), X /= H]].

filter_out_unavailable(Releases) when is_list(Releases) ->
    [{_LowestPrice, NumForSale, _Notes, _ReleaseYear} || 
        {_LowestPrice, NumForSale, _Notes, _ReleaseYear} <- Releases, 
        NumForSale > 0].

fmt_string(Template, Args) when is_list(Args) ->
    lists:flatten(io_lib:format(Template, Args)).
