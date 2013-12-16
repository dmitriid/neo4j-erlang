%%%-------------------------------------------------------------------
%%% @doc This is a very thin wrapper Neo4j REST API
%%%
%%%      Documentation: http://docs.neo4j.org/chunked/stable/rest-api.html
%%%
%%% @author Dmitrii Dimandt
%%% @copyright (C) 2013 Dmitrii Dimandt
%%%-------------------------------------------------------------------
-module(neo4j).
-author("dmitrii.dimandt").


%%_* Exports ===================================================================
-export([
        %% General
          connect/1
        , get_relationship_types/1
        , get_nodes_by_label/2
        , get_nodes_by_label/3
        , get_labels/1
        %% Transactions
        , transaction_begin/2
        , transaction_execute/2
        , transaction_commit/1
        , transaction_commit/2
        , transaction_execute_commit/2
        , transaction_rollback/1
        %% Cypher
        , cypher/2
        , cypher/3
        %% Nodes
        , create_node/1
        , create_node/2
        , get_node/2
        , delete_node/1
        , get_node_properties/1
        , set_node_properties/2
        , get_node_property/2
        , set_node_property/3
        , delete_node_properties/1
        , delete_node_property/2
        , get_relationships/2
        , get_typed_relationships/2
        , get_typed_relationships/3
        , add_node_labels/2
        , set_node_labels/2
        , delete_node_label/2
        , get_node_labels/1
        %% Relationships
        , get_relationship/2
        , create_relationship/3
        , create_relationship/4
        , delete_relationship/1
        , get_relationship_properties/1
        , set_relationship_properties/2
        , get_relationship_property/2
        , set_relationship_property/3
        , delete_relationship_properties/1
        , delete_relationship_property/2
        %% Indices
        , create_index/3
        , list_indexes/2
        , drop_index/3
        %% Constraints
        , create_constraint/3
        , get_constraint/3
        , get_uniqueness_constraints/2
        , get_label_constraints/2
        , get_constraints/1
        , drop_constraint/3
        %% Legacy node indices
        , create_node_index/2
        , create_node_index/3
        , delete_node_index/2
        , node_indices/1
        , add_node_to_index/5
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

%% -record(neo4j_node, { extensions
%%                     , paged_traverse
%%                     , outgoing_relationships
%%                     , traverse
%%                     , all_typed_relationships
%%                     , property
%%                     , all_relationships
%%                     , self
%%                     , outgoing_typed_relationships
%%                     , properties
%%                     , incoming_relationships
%%                     , incoming_typed_relationships
%%                     , create_relationship
%%                     , data
%%                     , labels
%%                     }
%%       ).
%%
%% -record(neo4j_relationship, { extensions
%%                             , start
%%                             , property
%%                             , self
%%                             , properties
%%                             , type
%%                             , 'end'
%%                             , data
%%                             }
%%       ).
%%
%% -record(neo4j_index, { template
%%                      , type
%%                      , provider
%%                      }
%%        ).
%%
%% -record(cypher_result, { columns :: list()
%%                        , data    :: [[neo4j_node() | neo4j_relationship() | binary()]]
%%                        }).

-type neo4j_root() :: proplists:proplist().
-type neo4j_node() :: proplists:proplist().
-type neo4j_relationship() :: proplists:proplist().
-type cypher_result() :: proplists:proplist().
-type neo4j_id() :: proplists:proplist().
-type neo4j_index() :: proplists:proplist().
-type neo4j_transaction() :: proplists:proplist().
-type neo4j_transaction_query() :: [ Query::binary()
                                   | {Query::binary(), Parameters::binary()}
                                   | {Query :: binary(), Parameters :: binary(), DataFormats::[binary()]}
                                   ].

-type neo4j_type() :: proplists:proplist().


%%_* API =======================================================================

%%_* General -------------------------------------------------------------------

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
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-relationship-types.html
%%
-spec get_relationship_types(neo4j_root()) -> [binary()] | {error, term()}.
get_relationship_types(Neo) ->
  {_, URI} = lists:keyfind(<<"relationship_types">>, 1, Neo),
  retrieve(URI).

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-node-labels.html#rest-api-get-all-nodes-with-a-label
%%
-spec get_nodes_by_label(neo4j_root(), binary()) -> [neo4j_node()] | {error, term()}.
get_nodes_by_label(Neo, Label) ->
  {_, URI} = lists:keyfind(<<"label">>, 1, Neo),
  retrieve(<<URI/binary, "/", Label/binary, "/nodes">>).

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-node-labels.html#rest-api-list-all-labels
%%
-spec get_labels(neo4j_root()) -> [binary()] | {error, term()}.
get_labels(Neo) ->
  {_, URI} = lists:keyfind(<<"labels">>, 1, Neo),
  retrieve(URI).

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-node-labels.html#rest-api-get-nodes-by-label-and-property
%%
-spec get_nodes_by_label(neo4j_root(), binary(), proplists:proplist()) -> [neo4j_node()] | {error, term()}.
get_nodes_by_label(Neo, Label, Properties) ->
  {_, URI} = lists:keyfind(<<"label">>, 1, Neo),
  Props = encode_query_string(Properties),
  retrieve(<<URI/binary, "/", Label/binary, "/nodes", "?", Props/binary>>).

%%_* Transactions --------------------------------------------------------------

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-transactional.html#rest-api-begin-a-transaction
%%      http://docs.neo4j.org/chunked/stable/rest-api-transactional.html#rest-api-execute-statements-in-an-open-transaction-in-rest-format-for-the-return
%%      http://docs.neo4j.org/chunked/stable/rest-api-transactional.html#rest-api-return-results-in-graph-format
%%
%%      Neo = neo4j:connect([{base_uri, BaseUri}]),
%%      neo4j:transaction_begin( Neo
%%                             , [ { <<"CREATE (n {props}) RETURN n">>
%%                                 , [{<<"props">>, <<"My Node">>}]
%%                                 , [<<"REST>>"] %% optional parameter for data format
%%                                 }
%%                               ]
%%                             )...
%%
-spec transaction_begin(neo4j_root(), neo4j_transaction_query()) -> proplists:proplist() | {error, term()}.
transaction_begin(Neo, Query) ->
  {_, URI} = lists:keyfind(<<"transaction">>, 1, Neo),
  Payload = encode_transaction_query(Query),
  create(URI, Payload).

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-transactional.html#rest-api-execute-statements-in-an-open-transaction
%%      http://docs.neo4j.org/chunked/stable/rest-api-transactional.html#rest-api-execute-statements-in-an-open-transaction-in-rest-format-for-the-return
%%      http://docs.neo4j.org/chunked/stable/rest-api-transactional.html#rest-api-return-results-in-graph-format
%%
%%      Neo = neo4j:connect([{base_uri, BaseUri}]),
%%      T = neo4j:transaction_begin( Neo
%%                                 , [ { <<"CREATE (n {props}) RETURN n">>
%%                                     , [{<<"props">>, <<"My Node">>}]
%%                                     , [<<"REST>>"] %% optional parameter for data format
%%                                     }
%%                                   ]
%%                                 , [<<"REST>>"] %% optional parameter for data format
%%                                 ),
%%      T1 = neo4j:transaction_execute(T, Query)...
%%
%%
%%      http://docs.neo4j.org/chunked/stable/rest-api-transactional.html#rest-api-reset-transaction-timeout-of-an-open-transaction
%%
%%      To reset transaction timeout, send in an emtpy query:
%%
%%      neo4j:transaction_execute(T1, <<"">>).
%%
-spec transaction_execute(neo4j_transaction(), neo4j_transaction_query()) -> proplists:proplist() | {error, term()}.
transaction_execute(T, Query) ->
  {_, URI0} = lists:keyfind(<<"commit">>, 1, T),
  URI = binary:part(URI0, {0, byte_size(URI0) - 7}),
  Payload = encode_transaction_query(Query),
  create(URI, Payload).

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-transactional.html#rest-api-commit-an-open-transaction
%%      http://docs.neo4j.org/chunked/stable/rest-api-transactional.html#rest-api-execute-statements-in-an-open-transaction-in-rest-format-for-the-return
%%      http://docs.neo4j.org/chunked/stable/rest-api-transactional.html#rest-api-return-results-in-graph-format
%%
-spec transaction_commit(neo4j_transaction()) -> proplists:proplist() | {error, term()}.
transaction_commit(T) ->
  {_, URI} = lists:keyfind(<<"commit">>, 1, T),
  create(URI).

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-transactional.html#rest-api-commit-an-open-transaction
%%      http://docs.neo4j.org/chunked/stable/rest-api-transactional.html#rest-api-execute-statements-in-an-open-transaction-in-rest-format-for-the-return
%%      http://docs.neo4j.org/chunked/stable/rest-api-transactional.html#rest-api-return-results-in-graph-format
%%
-spec transaction_commit(neo4j_transaction(), neo4j_transaction_query()) -> proplists:proplist() | {error, term()}.
transaction_commit(T, Query) ->
  {_, URI} = lists:keyfind(<<"commit">>, 1, T),
  Payload = encode_transaction_query(Query),
  create(URI, Payload).

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-transactional.html#rest-api-rollback-an-open-transaction
%%
-spec transaction_rollback(neo4j_transaction()) -> proplists:proplist() | {error, term()}.
transaction_rollback(T) ->
  {_, URI0} = lists:keyfind(<<"commit">>, 1, T),
  URI = binary:part(URI0, {0, byte_size(URI0) - 7}),
  delete(URI).

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-transactional.html#rest-api-begin-and-commit-a-transaction-in-one-request
%%      http://docs.neo4j.org/chunked/stable/rest-api-transactional.html#rest-api-execute-statements-in-an-open-transaction-in-rest-format-for-the-return
%%      http://docs.neo4j.org/chunked/stable/rest-api-transactional.html#rest-api-return-results-in-graph-format
%%
%%      Neo = neo4j:connect([{base_uri, BaseUri}]),
%%      neo4j:transaction_execute_commit( Neo
%%                                      , [ { <<"CREATE (n {props}) RETURN n">>
%%                                          , [{<<"props">>, <<"My Node">>}]
%%                                          , [<<"REST>>"] %% optional parameter for data format
%%                                          }
%%                                        ]
%%                                      )...
%%
-spec transaction_execute_commit(neo4j_root(), neo4j_transaction_query()) -> proplists:proplist() | {error, term()}.
transaction_execute_commit(Neo, Query) ->
  {_, URI} = lists:keyfind(<<"transaction">>, 1, Neo),
  Payload = encode_transaction_query(Query),
  create(<<URI/binary, "/commit">>, Payload).

%%_* Cypher --------------------------------------------------------------------

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
  create(URI, Payload).

%%_* Nodes ---------------------------------------------------------------------

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-nodes.html#rest-api-create-node
%%
-spec create_node(neo4j_root()) -> neo4j_node().
create_node(Neo) ->
  {_, URI} = lists:keyfind(<<"node">>, 1, Neo),
  create(URI).

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-nodes.html#rest-api-create-node-with-properties
%%
-spec create_node(neo4j_root(), proplists:proplist()) -> neo4j_node().
create_node(Neo, Props) ->
  {_, URI} = lists:keyfind(<<"node">>, 1, Neo),
  Payload = jsonx:encode(Props),
  create(URI, Payload).

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-nodes.html#rest-api-get-node
%%
-spec get_node( neo4j_root()
              , neo4j_id()
              ) -> neo4j_node() | {error, term()}.
get_node(_Neo, [{_,_}|_] = Node) ->
  Node;
get_node(Neo, Id) when is_binary(Id) ->
  case is_uri(Id) of
    true  ->
      retrieve(Id);
    false ->
      {_, URI} = lists:keyfind(<<"node">>, 1, Neo),
      retrieve(<<URI/binary, "/", Id/binary>>)
  end;
get_node(Neo, Id0) ->
  case id_to_binary(Id0) of
    {error, Reason} -> {error, Reason};
    Id              -> get_node(Neo, Id)
  end.

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-nodes.html#rest-api-delete-node
%%
-spec delete_node(neo4j_node()) -> ok | {error, term()}.
delete_node(Node) ->
  {_, URI} = lists:keyfind(<<"self">>, 1, Node),
  delete(URI).

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-node-properties.html#rest-api-get-properties-for-node
%%
-spec get_node_properties(neo4j_node() | neo4j_id()) -> proplists:proplist() | {error, term()}.
get_node_properties(Node) ->
  {_, URI} = lists:keyfind(<<"properties">>, 1, Node),
  retrieve(URI).

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-node-properties.html#rest-api-update-node-properties
%%
-spec set_node_properties( neo4j_node()
                         , proplist:proplist()) -> ok | {error, term()}.
set_node_properties(Node, Props) ->
  {_, URI} = lists:keyfind(<<"properties">>, 1, Node),
  update(URI, jsonx:encode(Props)).

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-node-properties.html
%%
-spec get_node_property(neo4j_node(), binary()) -> term() | {error, term()}.
get_node_property(Node, Prop) when is_binary(Prop) ->
  {_, URI} = lists:keyfind(<<"property">>, 1, Node),
  retrieve(replace_param(URI, <<"key">>, Prop));
get_node_property(_, _) ->
  {error, invalid_property}.

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-node-properties.html#rest-api-set-property-on-node
%%
-spec set_node_property( neo4j_node()
                       , binary()
                       , term()
                       ) -> ok | {error, term()}.
set_node_property(Node, Prop, Val) when is_binary(Prop) ->
  {_, URI} = lists:keyfind(<<"properties">>, 1, Node),
  Payload = jsonx:encode(Val),
  update(<<URI/binary, "/", Prop/binary, "/">>, Payload);
set_node_property(_, _, _) ->
  {error, invalid_property}.

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-node-properties.html#rest-api-delete-all-properties-from-node
%%
-spec delete_node_properties(neo4j_node()) -> ok | {error, term()}.
delete_node_properties(Node) ->
  {_, URI} = lists:keyfind(<<"properties">>, 1, Node),
  delete(URI).


%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-node-properties.html#rest-api-delete-a-named-property-from-a-node
%%
-spec delete_node_property(neo4j_node(), binary()) -> ok | {error, term()}.
delete_node_property(Node, Prop) when is_binary(Prop) ->
  {_, URI} = lists:keyfind(<<"properties">>, 1, Node),
  delete(<<URI/binary, "/", Prop/binary>>);
delete_node_property(_, _) ->
  {error, invalid_property}.

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-relationships.html#rest-api-get-all-relationships
%%
-spec get_relationships( neo4j_node()
                       , all | in | out
                       ) -> [neo4j_relationship()] | {error, term()}.
get_relationships(Node, Direction) ->
    get_relationship_by_direction(Node, Direction).

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-relationships.html#rest-api-get-all-relationships
%%
-spec get_typed_relationships( neo4j_node()
                             , binary()
                             ) -> [neo4j_relationship()] | {error, term()}.
get_typed_relationships(Node, Type) ->
  get_relationship_by_type(Node, Type).

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-relationships.html#rest-api-get-typed-relationships
%%
-spec get_typed_relationships( neo4j_node()
                             , binary()
                             , all | in | out
                             ) -> [neo4j_relationship()] | {error, term()}.
get_typed_relationships(Node, Type, Direction) ->
  get_relationship_by_type(Node, Type, Direction).

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-node-labels.html#rest-api-adding-a-label-to-a-node
%%      http://docs.neo4j.org/chunked/stable/rest-api-node-labels.html#rest-api-adding-multiple-labels-to-a-node
%%
-spec add_node_labels( neo4j_node()
                     , binary() | [binary()]
                     ) -> ok | {error, term()}.
add_node_labels(Node, Labels) ->
  {_, URI} = lists:keyfind(<<"labels">>, 1, Node),
  create(URI, jsonx:encode(Labels)).

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-node-labels.html#rest-api-replacing-labels-on-a-node
%%
-spec set_node_labels( neo4j_node()
                     , binary() | [binary()]
                     ) -> ok | {error, term()}.
set_node_labels(Node, Labels) ->
  {_, URI} = lists:keyfind(<<"labels">>, 1, Node),
  update(URI, jsonx:encode(Labels)).

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-node-labels.html#rest-api-removing-a-label-from-a-node
%%
-spec delete_node_label( neo4j_node()
                       , binary()
                       ) -> ok | {error, term()}.
delete_node_label(Node, Label) ->
  {_, URI} = lists:keyfind(<<"labels">>, 1, Node),
  delete(<<URI/binary, "/", Label/binary>>).

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-node-labels.html#rest-api-listing-labels-for-a-node
%%
-spec get_node_labels(neo4j_node()) -> ok | {error, term()}.
get_node_labels(Node) ->
  {_, URI} = lists:keyfind(<<"labels">>, 1, Node),
  retrieve(URI).

%%_* Relationships--------------------------------------------------------------

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-relationships.html#rest-api-get-typed-relationships
%%
-spec get_relationship(neo4j_root(), neo4j_id() | neo4j_relationship()) -> neo4j_relationship() | {error, term()}.
get_relationship(_Neo, [{_,_}|_] = Relationship) ->
  Relationship;
get_relationship(Neo, Id) when is_binary(Id) ->
  case is_uri(Id) of
    true -> retrieve(Id);
    false ->
      {_, URI} = lists:keyfind(<<"relationship">>, 1, Neo),
      retrieve(<<URI/binary, "/", Id/binary>>)
  end;
get_relationship(Neo, Id0) ->
  case id_to_binary(Id0) of
    {error, Reason} -> {error, Reason};
    Id              -> get_relationship(Neo, Id)
  end.

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-relationships.html#rest-api-create-relationship
%%
-spec create_relationship( neo4j_node()
                         , neo4j_node()
                         , binary()
                         ) -> neo4j_relationship().
create_relationship(FromNode, ToNode, Type) ->
  {_, CreateURI} = lists:keyfind(<<"create_relationship">>, 1, FromNode),
  {_, ToURI} = lists:keyfind(<<"self">>, 1, ToNode),
  Payload = jsonx:encode([ {<<"to">>, ToURI}
                         , {<<"type">>, Type}
                         ]),
  create(CreateURI, Payload).

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-relationships.html#rest-api-create-a-relationship-with-properties
%%
-spec create_relationship( neo4j_node()
                         , neo4j_node()
                         , binary()
                         , proplists:proplist()
                         ) -> neo4j_relationship().
create_relationship(FromNode, ToNode, Type, Props) ->
  {_, CreateURI} = lists:keyfind(<<"create_relationship">>, 1, FromNode),
  {_, ToURI} = lists:keyfind(<<"self">>, 1, ToNode),
  Payload = jsonx:encode([ {<<"to">>, ToURI}
                         , {<<"type">>, Type}
                         , {<<"data">>, Props}
                         ]),
  create(CreateURI, Payload).

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-nodes.html#rest-api-delete-node
%%
-spec delete_relationship(neo4j_relationship()) -> ok | {error, term()}.
delete_relationship(Relationship) ->
  {_, URI} = lists:keyfind(<<"self">>, 1, Relationship),
  delete(URI).

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-relationships.html#rest-api-get-all-properties-on-a-relationship
%%
-spec get_relationship_properties(neo4j_relationship() | neo4j_id()) -> proplists:proplist() | {error, term()}.
get_relationship_properties(Relationship) ->
  {_, URI} = lists:keyfind(<<"properties">>, 1, Relationship),
  retrieve(URI).

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-relationships.html#rest-api-set-all-properties-on-a-relationship
%%
-spec set_relationship_properties( neo4j_relationship()
                                 , proplist:proplist()) -> ok | {error, term()}.
set_relationship_properties(Relationship, Props) ->
  {_, URI} = lists:keyfind(<<"properties">>, 1, Relationship),
  update(URI, jsonx:encode(Props)).

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-relationships.html#rest-api-get-all-properties-on-a-relationship
%%
-spec get_relationship_property( neo4j_relationship()
                               , binary()
                               ) -> proplists:proplist() | {error, term()}.
get_relationship_property(Relationship, Prop) when is_binary(Prop) ->
  {_, URI} = lists:keyfind(<<"property">>, 1, Relationship),
  retrieve(replace_param(URI, <<"key">>, Prop));
get_relationship_property(_, _) ->
  {error, invalid_property}.

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-relationships.html#rest-api-set-single-property-on-a-relationship
%%
-spec set_relationship_property( neo4j_relationship()
                               , binary()
                               , term()
                               ) -> ok | {error, term()}.
set_relationship_property(Relationship, Prop, Val) when is_binary(Prop) ->
      {_, URI} = lists:keyfind(<<"properties">>, 1, Relationship),
      Payload = jsonx:encode(Val),
      update(<<URI/binary, "/", Prop/binary, "/">>, Payload);
set_relationship_property(_, _, _) ->
  {error, invalid_property}.

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-relationship-properties.html#rest-api-remove-properties-from-a-relationship
%%
-spec delete_relationship_properties(neo4j_relationship()) -> ok | {error, term()}.
delete_relationship_properties(Relationship) ->
  {_, URI} = lists:keyfind(<<"properties">>, 1, Relationship),
  delete(URI).

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-relationship-properties.html#rest-api-remove-property-from-a-relationship
%%
-spec delete_relationship_property(neo4j_relationship()
                                  , binary()
                                  ) -> ok | {error, term()}.
delete_relationship_property(Relationship, Prop) when is_binary(Prop) ->
  {_, URI} = lists:keyfind(<<"properties">>, 1, Relationship),
  delete(<<URI/binary, "/", Prop/binary>>);
delete_relationship_property(_, _) ->
  {error, invalid_property}.

%%_* Indices -------------------------------------------------------------------

%%
%% @doc http://docs.neo4j.org/chunked/milestone/rest-api-schema-indexes.html#rest-api-create-index
%%
-spec create_index(neo4j_root(), binary(), [binary()]) -> propliststs:proplist() | {error, term()}.
create_index(Neo, Label, PropKeys) ->
  {_, URI} = lists:keyfind(<<"index">>, 1, Neo),
  Payload = jsonx:encode([{<<"property_keys">>, PropKeys}]),
  create(<<URI/binary, "/", Label/binary>>, Payload).

%%
%% @doc http://docs.neo4j.org/chunked/milestone/rest-api-schema-indexes.html#rest-api-drop-index
%%
-spec drop_index(neo4j_root(), binary(), binary()) -> propliststs:proplist() | {error, term()}.
drop_index(Neo, Label, Key) ->
  {_, URI} = lists:keyfind(<<"index">>, 1, Neo),
  delete(<<URI/binary, "/", Label/binary, "/", Key/binary>>).

%%
%% @doc http://docs.neo4j.org/chunked/milestone/rest-api-schema-indexes.html#rest-api-list-indexes-for-a-label
%%
-spec list_indexes(neo4j_root(), binary()) -> propliststs:proplist() | {error, term()}.
list_indexes(Neo, Label) ->
  {_, URI} = lists:keyfind(<<"index">>, 1, Neo),
  retrieve(<<URI/binary, "/", Label/binary>>).

%%_* Consraints ----------------------------------------------------------------

%%
%% @doc http://docs.neo4j.org/chunked/milestone/rest-api-schema-constraints.html#rest-api-create-uniqueness-constraint
%%
-spec create_constraint(neo4j_root(), binary(), [binary()]) -> propliststs:proplist() | {error, term()}.
create_constraint(Neo, Label, PropKeys) ->
  {_, URI} = lists:keyfind(<<"constraint">>, 1, Neo),
  Payload = jsonx:encode([{<<"property_keys">>, PropKeys}]),
  create(<<URI/binary, "/", Label/binary, "/uniqueness">>, Payload).

%%
%% @doc http://docs.neo4j.org/chunked/milestone/rest-api-schema-constraints.html#rest-api-get-a-specific-uniqueness-constraint
%%
-spec get_constraint(neo4j_root(), binary(), [binary()]) -> propliststs:proplist() | {error, term()}.
get_constraint(Neo, Label, PropKey) ->
  {_, URI} = lists:keyfind(<<"constraint">>, 1, Neo),
  retrieve(<<URI/binary, "/", Label/binary, "/uniqueness/", PropKey/binary>>).


%%
%% @doc http://docs.neo4j.org/chunked/milestone/rest-api-schema-constraints.html#rest-api-get-all-uniqueness-constraints-for-a-label
%%
-spec get_uniqueness_constraints(neo4j_root(), binary()) -> propliststs:proplist() | {error, term()}.
get_uniqueness_constraints(Neo, Label) ->
  {_, URI} = lists:keyfind(<<"constraint">>, 1, Neo),
  retrieve(<<URI/binary, "/", Label/binary, "/uniqueness">>).


%%
%% @doc http://docs.neo4j.org/chunked/milestone/rest-api-schema-constraints.html#rest-api-get-all-constraints-for-a-label
%%
-spec get_label_constraints(neo4j_root(), binary()) -> propliststs:proplist() | {error, term()}.
get_label_constraints(Neo, Label) ->
  {_, URI} = lists:keyfind(<<"constraint">>, 1, Neo),
  retrieve(<<URI/binary, "/", Label/binary>>).


%%
%% @doc http://docs.neo4j.org/chunked/milestone/rest-api-schema-constraints.html#rest-api-get-all-constraints
%%
-spec get_constraints(neo4j_root()) -> propliststs:proplist() | {error, term()}.
get_constraints(Neo) ->
  {_, URI} = lists:keyfind(<<"constraint">>, 1, Neo),
  retrieve(URI).


%%
%% @doc http://docs.neo4j.org/chunked/milestone/rest-api-schema-constraints.html#rest-api-drop-constraint
%%
-spec drop_constraint(neo4j_root(), binary(), [binary()]) -> propliststs:proplist() | {error, term()}.
drop_constraint(Neo, Label, PropKey) ->
  {_, URI} = lists:keyfind(<<"constraint">>, 1, Neo),
  delete(<<URI/binary, "/", Label/binary, "/uniqueness/", PropKey/binary>>).


%%_* Legacy node indices -------------------------------------------------------

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-indexes.html#rest-api-create-node-index
%%
-spec create_node_index(neo4j_root(), binary()) -> neo4j_index() | {error, term()}.
create_node_index(Neo, Name) when is_binary(Name) ->
  {_, URI} = lists:keyfind(<<"node_index">>, 1, Neo),
  Payload = jsonx:encode([{<<"name">>, Name}]),
  create(URI, Payload);
create_node_index(_, _) ->
  {error, invalid_index_name}.

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-indexes.html#rest-api-create-node-index
%%
-spec create_node_index(neo4j_root(), binary(), proplists:proplist()) -> neo4j_index() | {error, term()}.
create_node_index(Neo, Name, Config) when is_binary(Name) ->
  {_, URI} = lists:keyfind(<<"node_index">>, 1, Neo),
  Payload = jsonx:encode([ {<<"name">>, Name}
                         , {<<"config">>, Config}
                         ]
                        ),
  create(URI, Payload);
create_node_index(_, _, _) ->
  {error, invalid_index_name}.

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-indexes.html#rest-api-delete-node-index
%%
-spec delete_node_index(neo4j_root(), binary()) -> ok | {error, term()}.
delete_node_index(Neo, Name) when is_binary(Name) ->
  {_, URI} = lists:keyfind(<<"node_index">>, 1, Neo),
  delete(<<URI/binary, "/", Name/binary>>);
delete_node_index(_, _) ->
  {error, invalid_index_name}.

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-indexes.html#rest-api-list-node-indexes
%%
-spec node_indices(neo4j_root()) -> [{binary(), neo4j_index()}] | {error, term()}.
node_indices(Neo) ->
  {_, URI} = lists:keyfind(<<"node_index">>, 1, Neo),
  list(retrieve(URI)).

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-indexes.html#rest-api-add-node-to-index
%%
-spec add_node_to_index( neo4j_root()
                       , neo4j_node() | neo4j_id()
                       , Index::binary()
                       , Key::binary
                       , Value::binary
                       ) -> term() | {error, term()}.
add_node_to_index(Neo, Node0, Index, Key, Value) when is_binary(Index) ->
  case get_node(Neo, Node0) of
    {error, Reason} -> {error, Reason};
    Node ->
      {_, NodeURI} = lists:keyfind(<<"self">>, 1, Node),
      Payload = jsonx:encode([ {<<"uri">>, NodeURI}
                             , {<<"key">>, Key}
                             , {<<"value">>, Value}
                             ]
                            ),
      {_, URI} = lists:keyfind(<<"node_index">>, 1, Neo),
      create(<<URI/binary, "/", Index/binary>>, Payload)
  end;
add_node_to_index(_, _, _, _, _) ->
  {error, invalid_index_name}.

%%_* Legacy relationship indices ------------------------------------------------------

%%_* Internal ==================================================================

%%
%% http://docs.neo4j.org/chunked/stable/rest-api-service-root.html#rest-api-get-service-root
%%
-spec get_root(binary()) -> neo4j_root() | {error, term()}.
get_root(BaseURI) when is_list(BaseURI)   -> get_root(list_to_binary(BaseURI));
get_root(BaseURI) when is_binary(BaseURI) ->
  case hackney:request(get, BaseURI, headers()) of
    {error, Reason} -> {error, Reason};
    {ok, StatusCode, _, Client} when StatusCode /= 200 ->
      {ok, Body, _} = hackney:body(Client),
      {error, {non_200_response, StatusCode, Body}};
    {ok, _, _, Client} ->
      {ok, Body, _} = hackney:body(Client),
      case jsonx:decode(Body, [{format, proplist}]) of
        {error, E1, E2} -> {error, {E1, E2}};
        Root          ->
          %% we add some links as these are not returned by neo4j
          %% an we wouldn't want to recreate them over and over again
          [ {<<"base_uri">>, BaseURI}
          , {<<"relationship">>, <<BaseURI/binary, "relationship">>}
          , {<<"label">>, <<BaseURI/binary, "label">>}
          , {<<"labels">>, <<BaseURI/binary, "labels">>}
          , {<<"index">>, <<BaseURI/binary, "schema/index">>}
          , {<<"constraint">>, <<BaseURI/binary, "schema/constraint">>}
            | Root
          ]
      end
  end.

-spec create(binary()) -> neo4j_type() | binary() | [term()] | {error, term()}.
create(URI) ->
  io:format("[POST] ~p~n", [URI]),
  case hackney:request(post, URI, headers()) of
    {error, Reason} -> {error, Reason};
    {ok, 200, _, Client} ->
      {ok, Body, _} = hackney:body(Client),
      jsonx:decode(Body, [{format, proplist}]);
    {ok, 201, _, Client} ->
      {ok, Body, _} = hackney:body(Client),
      jsonx:decode(Body, [{format, proplist}]);
    {ok, 204, _, _} ->
      ok;
    {ok, Status, _, Client} ->
      process_response(URI, Status, Client)
  end.

-spec create(binary(), binary()) -> neo4j_type() | binary() | [term()] | {error, term()}.
create(URI, Payload) ->
  io:format("[POST] ~p ~p~n", [URI, Payload]),
  case hackney:request(post, URI, headers(), Payload) of
    {error, Reason} -> {error, Reason};
    {ok, 200, _, Client} ->
      {ok, Body, _} = hackney:body(Client),
      jsonx:decode(Body, [{format, proplist}]);
    {ok, 201, _, Client} ->
      {ok, Body, _} = hackney:body(Client),
      jsonx:decode(Body, [{format, proplist}]);
    {ok, 204, _, _} ->
      ok;
    {ok, Status, _, Client} ->
      process_response(URI, Status, Client)
  end.

-spec retrieve(binary()) -> neo4j_type() | binary() | [term()] | {error, term()}.
retrieve(URI) ->
  io:format("[GET] ~p~n", [URI]),
  case hackney:request(get, URI, headers()) of
    {error, Reason} -> {error, Reason};
    {ok, 404, _, _} ->
      {error, not_found};
    {ok, 200, _, Client} ->
      {ok, Body, _} = hackney:body(Client),
      jsonx:decode(Body, [{format, proplist}]);
    {ok, 204, _, _} ->
      <<>>;
    {ok, Status, _, Client} ->
      process_response(URI, Status, Client)
  end.

-spec update(binary(), binary()) -> ok | {error, term()}.
update(URI, Payload) ->
  io:format("[PUT] ~p ~p~n", [URI, Payload]),
  case hackney:request(put, URI, headers(), Payload) of
    {error, Reason} -> {error, Reason};
    {ok, 204, _, _} ->
      ok;
    {ok, Status, _, Client} ->
      process_response(URI, Status, Client)
  end.

-spec delete(binary()) -> ok | {error, term()}.
delete(URI) ->
  io:format("[DELETE] ~p~n", [URI]),
  case hackney:request(delete, URI) of
    {error, Reason} -> {error, Reason};
    {ok, 204, _, _} ->
      ok;
    {ok, 200, _, Client} ->
      {ok, Body, _} = hackney:body(Client),
      jsonx:decode(Body, [{format, proplist}]);
    {ok, Status, _, Client} ->
      process_response(URI, Status, Client)
  end.

%%
%% @doc http://docs.neo4j.org/chunked/stable/rest-api-relationships.html#rest-api-get-all-relationships
%%
-spec get_relationship_by_direction(neo4j_node(), all | in | out) -> [neo4j_relationship()] | {error, term()}.
get_relationship_by_direction(Node, all) ->
  {_, URI} = lists:keyfind(<<"all_relationships">>, 1, Node),
  retrieve(URI);
get_relationship_by_direction(Node, in) ->
  {_, URI} = lists:keyfind(<<"incoming_relationships">>, 1, Node),
  retrieve(URI);
get_relationship_by_direction(Node, out) ->
  {_, URI} = lists:keyfind(<<"outgoing_relationships">>, 1, Node),
  retrieve(URI);
get_relationship_by_direction(_, _) ->
  {error, invalid_relationship_direction}.

-spec get_relationship_by_type(neo4j_node(), binary()) -> [neo4j_relationship()] | {error, term()}.
get_relationship_by_type(Node, Type) ->
  get_relationship_by_type(Node, Type, all).

-spec get_relationship_by_type( neo4j_node()
                              , binary()
                              , all | in | out
                              ) -> [neo4j_relationship()] | {error, term()}.
get_relationship_by_type(Node, Type, all) ->
  {_, URI} = lists:keyfind(<<"all_typed_relationships">>, 1, Node),
  retrieve(replace_param(URI, <<"-list|&|types">>, Type));
get_relationship_by_type(Node, Type, in) ->
  {_, URI} = lists:keyfind(<<"incoming_typed_relationships">>, 1, Node),
  retrieve(replace_param(URI, <<"-list|&|types">>, Type));
get_relationship_by_type(Node, Type, out) ->
  {_, URI} = lists:keyfind(<<"outgoing_typed_relationships">>, 1, Node),
  retrieve(replace_param(URI, <<"-list|&|types">>, Type));
get_relationship_by_type(_, _, _) ->
  {error, invalid_relationship_direction_or_type}.

%%
%% @doc
%% {
%%   "statements" : [ {
%%     "statement" : "CREATE (n {props}) RETURN n",
%%     "parameters" : {
%%       "props" : {
%%         "name" : "My Node"
%%       }
%%     }
%%   } ]
%% }
%%
-spec encode_transaction_query(proplists:proplist()) -> binary().
encode_transaction_query(<<"">>) ->
  jsonx:encode([{<<"statements">>, []}]);
encode_transaction_query(Q) ->
  Statements = [prepare_statement(Query) || Query <- Q],
  jsonx:encode([{<<"statements">>, Statements}]).

-spec prepare_statement(binary()) -> proplists:proplist();
                       ({binary()}) -> proplists:proplist();
                       ({binary(), proplists:proplist()}) -> proplists:proplist();
                       ({binary(), proplists:proplist(), [binary()]}) -> proplists:proplist().
prepare_statement({Q}) ->
  prepare_statement({Q, [], []});
prepare_statement({Q, P}) ->
  prepare_statement({Q, P, []});
prepare_statement({_, _, _} = Q) ->
  PrepareParams = fun(P) ->
                    [{Key, [{<<"name">>, Value}]} || {Key, Value} <- P]
                  end,
  prepare_statement(Q, PrepareParams);
prepare_statement(Q) when is_binary(Q) ->
  prepare_statement({Q, [], []}).

-spec prepare_statement( {binary(), proplists:proplist(), [binary()]}
                       , fun((proplists:proplist()) -> proplists:proplist())
                       ) -> proplists:proplist().
prepare_statement({Query, [], []}, _) ->
  [{<<"statement">>, Query}];
prepare_statement({Query, Params, []}, PrepareParams) ->
  [{<<"statement">>, Query}
  , {<<"parameters">>, PrepareParams(Params)}
  ];
prepare_statement({Query, [], Formats}, _) ->
  [{<<"statement">>, Query}
  , {<<"resultDataContents">>, Formats}
  ];
prepare_statement({Query, Params, Formats}, PrepareParams) ->
  [{<<"statement">>, Query}
  , {<<"parameters">>, PrepareParams(Params)}
  , {<<"resultDataContents">>, Formats}
  ].

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

-spec is_uri(binary()) -> boolean().
is_uri(<<$h, $t, $t, $p, _/binary>>) -> true;
is_uri(_)                            -> false.

-spec process_response(integer(), binary(), hackney:client()) -> {error, term()}.
process_response(URI, 404, _Client) ->
  {error, {not_found, URI}};
process_response(URI, 405, _Client) ->
  {error, {method_not_allowed, URI}};
process_response(URI, Status, Client) ->
  {ok, Body, _} = hackney:body(Client),
  case Body of
    <<>> -> {error, {Status, URI, <<>>}};
    _ -> {error, {Status, URI, jsonx:decode(Body, [{format, proplist}])}}
  end.

-spec list(list() | binary()) -> list().
list(L) when is_list(L)   -> L;
list(B) when is_binary(B) -> binary_to_list(B).

-spec encode_query_string(proplists:proplist()) -> binary().
encode_query_string(Props) ->
  List = [<<Key/binary, "=", Value/binary>> || {Key, Value} <- Props],
  Join = fun(B, <<>>) ->
              B;
            (B, Acc) ->
              <<Acc/binary, "&", B/binary>>
         end,
  lists:foldl(Join, <<>>, List).

-spec headers() -> proplists:proplist().
headers() ->
  [ {<<"Accept">>, <<"application/json; charset=UTF-8">>}
  , {<<"Content-Type">>, <<"application/json">>}
  ].
