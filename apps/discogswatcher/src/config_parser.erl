-module(config_parser).

-export([parse_json/1]).

parse_json(FilePath) ->
    yamerl_constr:file(FilePath, [{schema, json}]).
