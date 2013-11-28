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
        , cypher/2
        , cypher/3
        , create_node/1
        , create_node/2
        , get_node/2
        , delete_node/2
        , get_node_properties/2
        , set_node_properties/2
        , get_relationship/2
        , create_relationship/4
        , create_relationship/5
        , delete_relationship/2
        , get_relationship_properties/2
        , set_relationship_properties/2
        ]).

%%_* Defines ===================================================================

-record(neo4j_root, { extensions
                    , node
                    , reference_node
                    , node_index
                    , relationship_index
                    , extensions_info
                    , relationship_types
                    , batch
                    , cypher
                    , transaction
                    , neo4j_version
                    }
       ).

-record(neo4j_node, { extensions
                    , paged_traverse
                    , outgoing_relationships
                    , traverse
                    , all_typed_relationships
                    , property
                    , all_relationships
                    , self
                    , outgoing_typed_relationships
                    , properties
                    , incoming_relationships
                    , incoming_typed_relationships
                    , create_relationship
                    , data
                    , labels
                    }
      ).

-record(neo4j_relationship, { extensions
                            , start
                            , property
                            , self
                            , properties
                            , type
                            , 'end'
                            , data
                            }
      ).

-record(cypher_result, { columns :: list()
                       , data    :: [[neo4j_node() | neo4j_relationship() | binary()]]
                       }).

-type neo4j_root() :: #neo4j_root{}.
-type neo4j_node() :: #neo4j_node{}.
-type neo4j_relationship() :: #neo4j_relationship{}.
-type cypher_result() :: #cypher_result{}.
-type neo4j_id() :: integer() | string() | binary().


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

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-cypher.html#rest-api-send-a-query
%%
-spec cypher(neo4j_root(), binary()) -> cypher_result().
cypher(Neo, Query) ->
  cypher(Neo, Query, [{<<>>, <<>>}]).

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-cypher.html#rest-api-send-queries-with-parameters
%%
-spec cypher(neo4j_root(), binary(), proplists:proplist()) -> cypher_result().
cypher(Neo, Query, Params) ->
  {_, URI} = lists:keyfind(<<"cypher">>, 1, Neo),
  Payload = jsonx:encode([{query, Query}, {params, Params}]),
  io:format("~p~n~n", [Payload]),
  case hackney:request(post, URI, [], Payload) of
    {error, Reason} -> throw({error, Reason});
    {ok, StatusCode, _, Client} when StatusCode /= 200 ->
      {ok, Body, _} = hackney:body(Client),
      {error, jsonx:decode(Body, [{format, proplist}])};
    {ok, _, _, Client} ->
      {ok, Body, _} = hackney:body(Client),
      Decoder = jsonx:decoder( [ {cypher_result, record_info(fields, cypher_result)}
                               , {neo4j_node, record_info(fields, neo4j_node)}
                               , {neo4j_relationship, record_info(fields, neo4j_relationship)}
                               ]
                             , [{format, proplist}]),
      Decoder(Body)
  end.

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-nodes.html#rest-api-create-node
%%
-spec create_node(neo4j_root()) -> neo4j_node().
create_node(Neo) ->
  {_, URI} = lists:keyfind(<<"node">>, 1, Neo),
  create(Neo, URI).

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-nodes.html#rest-api-create-node-with-properties
%%
-spec create_node(neo4j_root(), proplists:proplist()) -> neo4j_node().
create_node(Neo, Props) ->
  {_, URI} = lists:keyfind(<<"node">>, 1, Neo),
  Payload = jsonx:encode(Props),
  create(Neo, URI, Payload).

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-nodes.html#rest-api-get-node
%%
-spec get_node( neo4j_root()
              , integer() | list() | binary()
              ) -> neo4j_node() | {error, not_found}.
get_node(Neo, Id0) ->
  case id_to_binary(Id0) of
    {error, Reason} -> {error, Reason};
    Id ->
      {_, URI} = lists:keyfind(<<"node">>, 1, Neo),
      retrieve(Neo, <<URI/binary, "/", Id/binary>>)
  end.

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-nodes.html#rest-api-delete-node
%%
-spec delete_node( neo4j_root()
                 , neo4j_id() | neo4j_node()) -> ok | {error, term()}.
delete_node(Neo, Node) ->
  case get_id(Node) of
    {error, Reason} -> {error, Reason};
    Id ->
      {_, URI} = lists:keyfind(<<"node">>, 1, Neo),
      delete(Neo, <<URI/binary, "/", Id/binary>>)
  end.

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-node-properties.html#rest-api-get-properties-for-node
%%
-spec get_node_properties(neo4j_root(), neo4j_node() | neo4j_id()) -> proplists:proplist() | {error, term()}.
get_node_properties(Neo, Node) ->
  {_, URI} = lists:keyfind(<<"node">>, 1, Neo),
  get_properties(Neo, URI, Node).

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-relationships.html#rest-api-get-relationship-by-id
%%
-spec get_relationship(neo4j_root(), neo4j_id()) -> neo4j_relationship().
get_relationship(Neo, Id0) ->
  case id_to_binary(Id0) of
    {error, Reason} -> {error, Reason};
    Id ->
      {_, URI} = lists:keyfind(<<"relationship">>, 1, Neo),
      retrieve(Neo, <<URI/binary, "/", Id/binary>>)
  end.

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-relationships.html#rest-api-create-relationship
%%
-spec create_relationship( neo4j_root()
                         , neo4j_node() | neo4j_id()
                         , neo4j_node() | neo4j_id()
                         , binary()
                         ) -> neo4j_relationship().
create_relationship(Neo, Node, To, Type) ->
  case {get_id(Node), get_id(To)} of
    {{error, Reason}, _} -> {error, Reason};
    {_, {error, Reason}} -> {error, Reason};
    {FromId, ToId} ->
      {_, URI} = lists:keyfind(<<"node">>, 1, Neo),
      Payload = jsonx:encode([ {<<"to">>, <<URI/binary, "/", ToId/binary>>}
                             , {<<"type">>, Type}
                             ]),
      create(Neo, <<URI/binary, "/", FromId/binary, "/relationships">>, Payload)
  end.

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-relationships.html#rest-api-create-a-relationship-with-properties
%%
-spec create_relationship( neo4j_root()
                         , neo4j_node() | neo4j_id()
                         , neo4j_node() | neo4j_id()
                         , binary()
                         , proplists:proplist()
                         ) -> neo4j_relationship().
create_relationship(Neo, Node, To, Type, Props) ->
  case {get_id(Node), get_id(To)} of
    {{error, Reason}, _} -> {error, Reason};
    {_, {error, Reason}} -> {error, Reason};
    {FromId, ToId} ->
      {_, URI} = lists:keyfind(<<"node">>, 1, Neo),
      Payload = jsonx:encode([ {<<"to">>, <<URI/binary, "/", ToId/binary>>}
                             , {<<"type">>, Type}
                             , {<<"data">>, Props}
                             ]),
      create(Neo, <<URI/binary, "/", FromId/binary, "/relationships">>, Payload)
  end.

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-nodes.html#rest-api-delete-node
%%
-spec delete_relationship( neo4j_root()
                         , neo4j_id() | neo4j_relationship()) -> ok | {error, term()}.
delete_relationship(Neo, Relationship) ->
  case get_id(Relationship) of
    {error, Reason} -> {error, Reason};
    Id ->
      {_, URI} = lists:keyfind(<<"relationship">>, 1, Neo),
      delete(Neo, <<URI/binary, "/", Id/binary>>)
  end.

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-relationships.html#rest-api-get-all-properties-on-a-relationship
%%
-spec get_relationship_properties( neo4j_root()
                                 , neo4j_relationship() | neo4j_id()) -> proplists:proplist() | {error, term()}.
get_relationship_properties(Neo, Relationship) ->
  {_, URI} = lists:keyfind(<<"relationship">>, 1, Neo),
  get_properties(Neo, URI, Relationship).

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-relationships.html#rest-api-set-all-properties-on-a-relationship
%%
-spec set_relationship_properties( neo4j_root()
                                 , neo4j_relationship() | neo4j_id()
                                 , proplist:proplist()) -> ok | {error, term()}.
set_relationship_properties(Neo, Relationship, Props) ->
  {_, URI} = lists:keyfind(<<"relationship">>, 1, Neo),
  set_properties(Neo, URI, Relationship, Props).

%%_* Internal ==================================================================

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-service-root.html#rest-api-get-service-root
%%
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
      case jsonx:decode(Body, [{format, proplist}]) of
        {error, E1, E2} -> {error, {E1, E2}};
        Record          ->
          Decoder = jsonx:decoder( [ {cypher_result, record_info(fields, cypher_result)}
                                   , {neo4j_node, record_info(fields, neo4j_node)}
                                   , {neo4j_relationship, record_info(fields, neo4j_relationship)}
                                   ]
                                 , [{format, proplist}]),
          [ {<<"base_uri">>, BaseURI}
          , {<<"relationship">>, <<BaseURI/binary, "relationship">>}
          , {<<"decoder">>, Decoder}
            | Record
          ]
      end
  end.

-spec create(neo4j_root(), binary()) -> neo4j_node() | neo4j_relationship() | {error, term()}.
create(Neo, URI) ->
  case hackney:request(post, URI) of
    {error, Reason} -> throw({error, Reason});
    {ok, StatusCode, _, Client} when StatusCode /= 201 ->
      {ok, Body, _} = hackney:body(Client),
      {error, jsonx:decode(Body, [{format, proplist}])};
    {ok, _, _, Client} ->
      {ok, Body, _} = hackney:body(Client),
      {_, Decoder} = lists:keyfind(<<"decoder">>, 1, Neo),
      Decoder(Body)
  end.

-spec create(neo4j_root(), binary(), binary()) -> neo4j_node() | neo4j_relationship() | {error, term()}.
create(Neo, URI, Payload) ->
  case hackney:request(post, URI, [], Payload) of
    {error, Reason} -> throw({error, Reason});
    {ok, StatusCode, _, Client} when StatusCode /= 201 ->
      {ok, Body, _} = hackney:body(Client),
      {error, jsonx:decode(Body, [{format, proplist}])};
    {ok, _, _, Client} ->
      {ok, Body, _} = hackney:body(Client),
      {_, Decoder} = lists:keyfind(<<"decoder">>, 1, Neo),
      Decoder(Body)
  end.

-spec retrieve(neo4j_root(), binary()) -> neo4j_node() | neo4j_relationship() | {error, term()}.
retrieve(Neo, URI) ->
  case hackney:request(get, URI) of
    {error, Reason} -> throw({error, Reason});
    {ok, StatusCode, _, _} when StatusCode == 404 ->
      {error, not_found};
    {ok, StatusCode, _, Client} when StatusCode /= 200 ->
      {ok, Body, _} = hackney:body(Client),
      {error, jsonx:decode(Body, [{format, proplist}])};
    {ok, _, _, Client} ->
      {ok, Body, _} = hackney:body(Client),
      {_, Decoder} = lists:keyfind(<<"decoder">>, 1, Neo),
      Decoder(Body)
  end.

-spec delete(neo4j_root(), binary()) -> ok | {error, term()}.
delete(_Neo, URI) ->
  case hackney:request(delete, URI) of
    {error, Reason} -> throw({error, Reason});
    {ok, StatusCode, _, _} when StatusCode == 404 ->
      ok;
    {ok, StatusCode, _, Client} when StatusCode /= 204 ->
      {ok, Body, _} = hackney:body(Client),
      {error, jsonx:decode(Body, [{format, proplist}])};
    {ok, _, _, _} ->
      ok
  end.

-spec get_properties( neo4j_root()
                    , binary()
                    , neo4j_node() | neo4j_relationship() | neo4j_id()
                    ) -> proplists:proplist() | {error, term()}.
get_properties(Neo, URI, Entity) ->
  case get_id(Entity) of
    {error, Reason} -> {error, Reason};
    Id ->
      retrieve(Neo, <<URI/binary, "/", Id/binary, "/properties">>)
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

-spec id_to_binary(neo4j_id()) -> binary() | {error, invalid_id}.
id_to_binary(Id) when is_integer(Id) ->
  integer_to_binary(Id, 10);
id_to_binary(Id) when is_list(Id) ->
  list_to_binary(Id);
id_to_binary(Id) when is_binary(Id) ->
  Id;
id_to_binary(_) ->
  {error, invalid_id}.

-spec get_id(neo4j_node()) -> binary().
get_id(#neo4j_node{self = URI}) ->
  id_to_binary(filename:basename(URI));
get_id(#neo4j_relationship{self = URI}) ->
  id_to_binary(filename:basename(URI));
get_id(Id) ->
  id_to_binary(Id).
