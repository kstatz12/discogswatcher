-module(discogs).
-include_lib("eunit/include/eunit.hrl").


-export([get_releases_for_record/3]).

-export([format_search_url/2]).
    
get_releases_for_record(HttpGet, Record, Token) ->
    Document = HttpGet(format_search_url(Record, Token)),
    Items = proplists:get_value("results", Document),
    MasterUrls = lists:map(fun(X) -> proplists:get_value("master_url", X) end, Items),
    get_releases(HttpGet, MasterUrls, Token).
    
    
get_releases(HttpGet, MasterUrls, Token) when is_list(MasterUrls) ->
    UniqueMasterUrls = utils:dedup(MasterUrls),
    MainReleaseIds = lists:map(fun(X) -> acc_master_releases(fun discogs:exec_request/1, X, Token) end, UniqueMasterUrls),
    Releases = lists:map(fun(X) -> get_release(HttpGet, X, Token) end, MainReleaseIds),
    filter_out_unavailable(Releases).

acc_master_releases(HttpExecute, Url, Token ) ->
    Document = HttpExecute(format_master_url(Url, Token)),
    proplists:get_value("main_release", Document).

get_release(HttpExecute, ReleaseId, Token) ->
    Url = format_release_url(ReleaseId,Token),
    Document = HttpExecute(Url),
    LowestPrice = proplists:get_value("lowest_price", Document),
    Notes = proplists:get_value("notes", Document),
    ReleaseYear = proplists:get_value("released_formatted", Document),
    NumForSale = proplists:get_value("num_for_sale", Document),
    {LowestPrice, NumForSale, Notes, ReleaseYear}.

format_master_url(Url, Token) ->
    Url = util:fmt_string("~?token=~s", [Url, Token]),
    list_to_binary(Url).

format_search_url(Record, Token) ->
    {Artist, Title, Format} = Record,
    BaseUrl = "https://api.discogs.com/database/search",
    Template = "~s?artist=~s&title=~s&format=~s&token=~s",
    Url = util:fmt_string(Template, [BaseUrl, Artist, Title, Format, Token]),
    list_to_binary(Url).

format_release_url(ReleaseId, Token) ->
    BaseUrl = "https://api.discogs.com/releases/~s?token=~s",
    util:fmt_string(BaseUrl, [ReleaseId, Token]).

filter_out_unavailable(Releases) when is_list(Releases) ->
    [{_LowestPrice, NumForSale, _Notes, _ReleaseYear} || 
        {_LowestPrice, NumForSale, _Notes, _ReleaseYear} <- Releases, 
        NumForSale > 0].

%% Tests %% 




