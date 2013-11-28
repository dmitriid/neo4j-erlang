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
        , set_node_properties/3
        , get_node_property/3
        , get_relationship/2
        , create_relationship/4
        , create_relationship/5
        , delete_relationship/2
        , get_relationship_properties/2
        , set_relationship_properties/3
        , get_relationship_property/3
        ]).

%%_* Defines ===================================================================

%% -record(neo4j_root, { extensions
%%                     , node
%%                     , reference_node
%%                     , node_index
%%                     , relationship_index
%%                     , extensions_info
%%                     , relationship_types
%%                     , batch
%%                     , cypher
%%                     , transaction
%%                     , neo4j_version
%%                     }
%%        ).

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

-type neo4j_root() :: proplists:proplist(). %%#neo4j_root{}.
-type neo4j_node() :: #neo4j_node{}.
-type neo4j_relationship() :: #neo4j_relationship{}.
-type cypher_result() :: #cypher_result{}.
-type neo4j_id() :: integer() | string() | binary().


%%_* API =======================================================================

-spec connect(proplists:proplist()) -> neo4j_root() | {error, base_uri_not_specified}.
connect([]) ->
  {error, base_uri_not_specified};
connect(Options) ->
  _ = start_app(hackney),
  case lists:keyfind(base_uri, 1, Options) of
    {_, BaseURI} -> get_root(BaseURI);
    _            -> {error, base_uri_not_specified}
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
-spec cypher(neo4j_root(), binary(), proplists:proplist()) -> cypher_result() | {error, term()}.
cypher(Neo, Query, Params) ->
  {_, URI} = lists:keyfind(<<"cypher">>, 1, Neo),
  Payload = jsonx:encode([{query, Query}, {params, Params}]),
  case hackney:request(post, URI, [], Payload) of
    {error, Reason} -> {error, Reason};
    {ok, StatusCode, _, Client} when StatusCode /= 200 ->
      {ok, Body, _} = hackney:body(Client),
      Reason = jsonx:decode(Body, [{format, proplist}]),
      {error, Reason};
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
              , neo4j_id()
              ) -> neo4j_node() | {error, not_found}.
get_node(_Neo, #neo4j_node{} = Node) ->
  Node;
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
  %% DELETE is ok even for 404 Not Found
  %% See https://github.com/for-GET/know-your-http-well/blob/master/methods.md
  case get_node(Neo, Node) of
    {error, not_found}      -> ok;
    {error, Reason}         -> {error, Reason};
    #neo4j_node{self = URI} -> delete(Neo, URI)
  end.

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-node-properties.html#rest-api-get-properties-for-node
%%
-spec get_node_properties(neo4j_root(), neo4j_node() | neo4j_id()) -> proplists:proplist() | {error, term()}.
get_node_properties(Neo, Node) ->
  case get_node(Neo, Node) of
    {error, Reason} -> {error, Reason};
    #neo4j_node{properties = URI} -> retrieve(Neo, URI)
  end.


%%
%% http://docs.neo4j.org/chunked/stable/rest-api-node-properties.html#rest-api-update-node-properties
%%
-spec set_node_properties( neo4j_root()
                         , neo4j_relationship() | neo4j_id()
                         , proplist:proplist()) -> ok | {error, term()}.
set_node_properties(Neo, Node, Props) ->
  case get_node(Neo, Node) of
    {error, Reason} -> {error, Reason};
    #neo4j_node{properties = URI} ->
      update(Neo, URI, jsonx:encode(Props))
  end.

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-node-properties.html
%%

-spec get_node_property(neo4j_root(), neo4j_node() | neo4j_id(), binary()) -> term() | {error, term()}.
get_node_property(Neo, Node, Prop) when is_binary(Prop) ->
  case get_node(Neo, Node) of
    {error, Reason} -> {error, Reason};
    #neo4j_node{property = URI} ->
      retrieve(Neo, replace_param(URI, <<"key">>, Prop))
  end;
get_node_property(_, _, _) ->
  {error, invalid_property}.

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-relationships.html#rest-api-get-relationship-by-id
%%
-spec get_relationship(neo4j_root(), neo4j_id() | neo4j_relationship()) -> neo4j_relationship().
get_relationship(_Neo, #neo4j_relationship{} = Relationship) ->
  Relationship;
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
create_relationship(Neo, From, To, Type) ->
  case {get_node(Neo, From), get_node(Neo, To)} of
    {{error, Reason}, _} -> {error, Reason};
    {_, {error, Reason}} -> {error, Reason};
    { #neo4j_node{create_relationship = CreateURI}
    , #neo4j_node{self = ToURI}
    } ->
      Payload = jsonx:encode([ {<<"to">>, ToURI}
                             , {<<"type">>, Type}
                             ]),
      create(Neo, CreateURI, Payload)
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
create_relationship(Neo, From, To, Type, Props) ->
  case {get_node(Neo, From), get_node(Neo, To)} of
    {{error, Reason}, _} -> {error, Reason};
    {_, {error, Reason}} -> {error, Reason};
    { #neo4j_node{create_relationship = CreateURI}
    , #neo4j_node{self = ToURI}
    } ->
      Payload = jsonx:encode([ {<<"to">>, ToURI}
                             , {<<"type">>, Type}
                             , {<<"data">>, Props}
                             ]),
      create(Neo, CreateURI, Payload)
  end.

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-nodes.html#rest-api-delete-node
%%
-spec delete_relationship( neo4j_root()
                         , neo4j_id() | neo4j_relationship()) -> ok | {error, term()}.
delete_relationship(Neo, Relationship) ->
  case get_relationship(Neo, Relationship) of
    {error, not_found}              -> ok;
    {error, Reason}                 -> {error, Reason};
    #neo4j_relationship{self = URI} -> delete(Neo, URI)
  end.

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-relationships.html#rest-api-get-all-properties-on-a-relationship
%%
-spec get_relationship_properties( neo4j_root()
                                 , neo4j_relationship() | neo4j_id()) -> proplists:proplist() | {error, term()}.
get_relationship_properties(Neo, Relationship) ->
  case get_relationship(Neo, Relationship) of
    {error, Reason} -> {error, Reason};
    #neo4j_relationship{properties = URI} ->
      retrieve(Neo, URI)
  end.

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-relationships.html#rest-api-set-all-properties-on-a-relationship
%%
-spec set_relationship_properties( neo4j_root()
                                 , neo4j_relationship() | neo4j_id()
                                 , proplist:proplist()) -> ok | {error, term()}.
set_relationship_properties(Neo, Relationship, Props) ->
  case get_relationship(Neo, Relationship) of
    {error, Reason} -> {error, Reason};
    #neo4j_relationship{properties = URI} ->
      update(Neo, URI, jsonx:encode(Props))
  end.

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-relationships.html#rest-api-get-all-properties-on-a-relationship
%%
-spec get_relationship_property( neo4j_root()
                               , neo4j_relationship() | neo4j_id()
                               , binary()
                               ) -> proplists:proplist() | {error, term()}.
get_relationship_property(Neo, Relationship, Prop) when is_binary(Prop) ->
  case get_relationship(Neo, Relationship) of
    {error, Reason} -> {error, Reason};
    #neo4j_relationship{property = URI} ->
      retrieve(Neo, replace_param(URI, <<"key">>, Prop))
  end;
get_relationship_property(_, _, _) ->
  {error, invalid_property}.


%%_* Internal ==================================================================

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-service-root.html#rest-api-get-service-root
%%
-spec get_root(binary()) -> neo4j_root() | {error, term()}.
get_root(BaseURI) when is_list(BaseURI)   -> get_root(list_to_binary(BaseURI));
get_root(BaseURI) when is_binary(BaseURI) ->
  case hackney:request(get, BaseURI, [{<<"Accept">>, <<"application/json">>}]) of
    {error, Reason} -> {error, Reason};
    {ok, StatusCode, _, Client} when StatusCode /= 200 ->
      {ok, Body, _} = hackney:body(Client),
      {error, {non_200_response, StatusCode, Body}};
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
  io:format("[POST] ~p~n", [URI]),
  case hackney:request(post, URI, headers()) of
    {error, Reason} -> {error, Reason};
    {ok, 201, _, Client} ->
      {ok, Body, _} = hackney:body(Client),
      {_, Decoder} = lists:keyfind(<<"decoder">>, 1, Neo),
      Decoder(Body);
    {ok, _, _, Client} ->
      {ok, Body, _} = hackney:body(Client),
      {error, jsonx:decode(Body, [{format, proplist}])}
  end.

-spec create(neo4j_root(), binary(), binary()) -> neo4j_node() | neo4j_relationship() | {error, term()}.
create(Neo, URI, Payload) ->
  io:format("[POST] ~p ~p~n", [URI, Payload]),
  case hackney:request(post, URI, headers(), Payload) of
    {error, Reason} -> {error, Reason};
    {ok, 201, _, Client} ->
      {ok, Body, _} = hackney:body(Client),
      {_, Decoder} = lists:keyfind(<<"decoder">>, 1, Neo),
      Decoder(Body);
    {ok, _, _, Client} ->
      {ok, Body, _} = hackney:body(Client),
      {error, jsonx:decode(Body, [{format, proplist}])}
  end.

-spec retrieve(neo4j_root(), binary()) -> neo4j_node() | neo4j_relationship() | {error, term()}.
retrieve(Neo, URI) ->
  io:format("[GET] ~p~n", [URI]),
  case hackney:request(get, URI, headers()) of
    {error, Reason} -> {error, Reason};
    {ok, 404, _, _} ->
      {error, not_found};
    {ok, 200, _, Client} ->
      {ok, Body, _} = hackney:body(Client),
      {_, Decoder} = lists:keyfind(<<"decoder">>, 1, Neo),
      Decoder(Body);
    {ok, _, _, Client} ->
      {ok, Body, _} = hackney:body(Client),
      {error, jsonx:decode(Body, [{format, proplist}])}
  end.

-spec update(neo4j_root(), binary(), binary()) -> ok | {error, term()}.
update(_Neo, URI, Payload) ->
  io:format("[PUT] ~p ~p~n", [URI, Payload]),
  case hackney:request(put, URI, headers(), Payload) of
    {error, Reason} -> {error, Reason};
    {ok, 204, _, _} ->
      ok;
    {ok, Stat, _, Client} ->
      io:format("~p~n~n", [Stat]),
      {ok, Body, _} = hackney:body(Client),
      io:format("~p~n~n", [Body]),
      {error, jsonx:decode(Body, [{format, proplist}])}
  end.

-spec delete(neo4j_root(), binary()) -> ok | {error, term()}.
delete(_Neo, URI) ->
  io:format("[DELETE] ~p~n", [URI]),
  case hackney:request(delete, URI) of
    {error, Reason} -> {error, Reason};
    {ok, 404, _, _} ->
      ok;
    {ok, 204, _, _} ->
      ok;
    {ok, _, _, Client} ->
      {ok, Body, _} = hackney:body(Client),
      {error, jsonx:decode(Body, [{format, proplist}])}
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

%%
%% Replace {key} type params in URIs provided by resources
%%
-spec replace_param(binary(), binary(), binary()) -> binary().
replace_param(URI, Param, Value) ->
  binary:replace(URI, <<"{", Param/binary, "}">>, Value).

headers() ->
  [ {<<"Accept">>, <<"application/json; charset=UTF-8">>}
  , {<<"Content-Type">>, <<"application/json">>}
  ].
