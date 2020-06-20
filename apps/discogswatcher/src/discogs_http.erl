-module(discogs_http).

-export([exec_get/1]).

exec_get(Url) ->
    {ok, _, _, Ref} = hackney:request(get, Url, [], <<>>, [{pool, default}]),
    {ok, Body} = hackney:body(Ref),
    Documents = yamerl_constr:string(Body),
    [Document | _] = Documents,
    Document.


