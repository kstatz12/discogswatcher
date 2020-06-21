-module(util).
-include_lib("eunit/include/eunit.hrl").

-export([dedup/1]).
-export([fmt_string/2]).

dedup([]) -> [];
dedup([H | T]) -> [H | [X || X <- dedup(T), X /= H]].

fmt_string(Template, Args) when is_list(Args) ->
    lists:flatten(io_lib:format(Template, Args)).

%% Tests %%
dedup_test() ->
    TestIntData = [1, 2, 3, 3, 4, 5],
    [1, 2, 3, 4, 5] = dedup(TestIntData),
    TestStrData = ["one", "two", "two", "three"],
    ["one", "two", "three"] = dedup(TestStrData).

fmt_string_test() ->
    Args = [1, 2],
    Template = "~wtest~w",
    "1test2" = fmt_string(Template, Args).




    



