-module(discogs_http).

-export([exec_get/1]).

exec_get(Url) ->
    case hackney:request(get, Url,[], <<>>, [{pool, default}] ) of
        {error, _} ->
            {error, nil};
        {ok, _, _, Ref} -> 
            {ok, Body} = hackney:body(Ref),
            Document = parse_body(Body),
            {ok, Document}
    end.

parse_body(Body) ->
    Documents = yamerl_constr:string(Body),
    [Document | _] = Documents,
    Document.

    


