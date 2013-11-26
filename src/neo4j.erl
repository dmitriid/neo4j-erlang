%%%-------------------------------------------------------------------
%%% @author Dmitrii Dimandt
%%% @copyright (C) 2013 Dmitrii Dimandt
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(neo4j).
-author("dmitrii.dimandt").


%%_* Exports ===================================================================
-export([ connect/1

        ]).

%%_* Defines ===================================================================

-record(neo4j_root, { extensions = {}
                    , node
                    , reference_node
                    , node_index
                    , relationship_index
                    , extensions_info
                    , relationship_types
                    , batch
                    , cypher
                    , neo4j_version
                    }
       ).

-type neo4j_root() :: #neo4j_root{}.

%%_* API =======================================================================

-spec connect(proplists:proplist()) -> neo4j_root().
connect([]) ->
  throw({error, base_uri_not_specified});
connect(Options) ->
  start_app(hackney),
  case lists:keyfind(base_uri, 1, Options) of
    {_, BaseURI} -> get_root(BaseURI);
    _            -> throw({error, base_uri_not_specified})
  end.


%%_* Internal ==================================================================

-spec get_root(binary()) -> neo4j_root().
get_root(BaseURI) when is_list(BaseURI)   -> get_root(list_to_binary(BaseURI));
get_root(BaseURI) when is_binary(BaseURI) ->
  case hackney:request(get, BaseURI) of
    {error, Reason} -> throw({error, Reason});
    {ok, StatusCode, _, Client} when StatusCode /= 200 ->
      {ok, Body, _} = hackney:body(Client),
      throw({error, {non_200_response, Body}});
    {ok, _, _, Client} ->
      {ok, Body, _} = hackney:body(Client),
      #neo4j_root{ node = Body }
  end.


%%_* Helpers ===================================================================

-spec start_app(atom()) -> [atom()].

start_app(App) ->
  lists:flatten(start_app(App, 30)).

-spec start_app(atom(), 0..30) -> [atom()].
start_app(_, 0) ->
  throw(too_much_recursion);
start_app(App, N) ->
  case application:start(App) of
    {error, {already_started, App}} ->
      [];
    {error, {not_started, OtherApp}} ->
      [start_app(OtherApp, N - 1),
        start_app(App, N - 1)];
    ok ->
      [App]
  end.
