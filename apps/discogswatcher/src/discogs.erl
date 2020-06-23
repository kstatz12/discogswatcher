-module(discogs).
-include_lib("eunit/include/eunit.hrl").

-export([get_releases_for_record/3]).

%% Public API %%
get_releases_for_record(HttpGet, Record, Token) ->
    case HttpGet(format_search_url(Record, Token)) of
        {error, _} ->
            {error, "Could Not get Releases"};
        {ok, Document} ->
            Items = proplists:get_value("results", Document),
            MasterUrls = lists:map(fun(X) -> proplists:get_value("master_url", X) end, Items),
            get_releases(HttpGet, MasterUrls, Token)
    end.

%% Private API Callers %%

get_releases(HttpGet, MasterUrls, Token) when is_list(MasterUrls) ->
    UniqueMasterUrls = utils:dedup(MasterUrls),
    MainReleaseIds = lists:map(fun(X) -> acc_master_releases(fun discogs:exec_request/1, X, Token) end, UniqueMasterUrls),
    Releases = lists:map(fun(X) -> get_release(HttpGet, X, Token) end, filter_out_nils(MainReleaseIds)),
    filter_out_unavailable(filter_out_nils(Releases)).

acc_master_releases(HttpGet, Url, Token ) ->
    case HttpGet(format_master_url(Url, Token)) of
        {error, _} ->
            nil;
        {ok, Document} ->
            proplists:get_value("main_release", Document)
        end.

get_release(HttpGet, ReleaseId, Token) ->
    Url = format_release_url(ReleaseId,Token),
    case HttpGet(Url) of
        {error, _} ->
            nil;
        {ok, Document} ->
            LowestPrice = proplists:get_value("lowest_price", Document),
            Notes = proplists:get_value("notes", Document),
            ReleaseYear = proplists:get_value("released_formatted", Document),
            NumForSale = proplists:get_value("num_for_sale", Document),
            {LowestPrice, NumForSale, Notes, ReleaseYear}
    end.

%% Filters %%
filter_out_nils(Items) ->
    [X || X <- Items, X /= nil].

filter_out_unavailable(Releases) when is_list(Releases) ->
    [{_LowestPrice, NumForSale, _Notes, _ReleaseYear} || 
        {_LowestPrice, NumForSale, _Notes, _ReleaseYear} <- Releases, 
        NumForSale > 0].

%% URL Formatters %%
format_master_url(Url, Token) ->
    FormattedUrl = util:fmt_string("~s?token=~s", [Url, Token]),
    list_to_binary(FormattedUrl).

format_search_url(Record, Token) ->
    {Artist, Title, Format} = Record,
    BaseUrl = "https://api.discogs.com/database/search",
    Template = "~s?artist=~s&title=~s&format=~s&token=~s",
    Url = util:fmt_string(Template, [BaseUrl, Artist, Title, Format, Token]),
    list_to_binary(Url).

format_release_url(ReleaseId, Token) ->
    BaseUrl = "https://api.discogs.com/releases/~s?token=~s",
    Url = util:fmt_string(BaseUrl, [ReleaseId, Token]),
    list_to_binary(Url).


%% Tests %% 
format_search_url_test() ->
    <<"https://api.discogs.com/database/search?artist=test&title=test&format=test&token=test">> = format_search_url({"test", "test", "test"}, "test").

format_master_url_test() ->
    <<"test?token=test">> = format_master_url("test", "test").
        
format_release_url_test() ->
    <<"https://api.discogs.com/releases/test?token=test">> = format_release_url("test", "test").

filter_out_unavailable_test() ->
    TestList = [
                {"", 1, "", ""},
                {"", 3, "", ""},
                {"", 0, "", ""}
               ],
    Result = filter_out_unavailable(TestList),
    2 = length(Result).

filter_out_nils_test() ->
    TestData = [
                1, 
                2, 
                nil,
                4
               ],
    FilteredList = filter_out_nils(TestData),
    3 = length(FilteredList).

