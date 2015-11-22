%%%-----------------------------------------------------------------------------
%%% @doc Test SUITE for neo4j module
%%%
%%%      Note: requires a neo4j running on default port
%%%
%%% @author Dmitrii Dimandt
%%% @copyright (C) 2013 Dmitrii Dimandt
%%%-----------------------------------------------------------------------------
-module(neo4j_SUITE).
-author("dmitrii.dimandt").

%%_* API =======================================================================
%% CT callbacks
-export([ all/0
        , init_per_testcase/2
        ]).
%% General
-export([ connect/1
        ]).

%% Nodes
-export([ create_node/1
        , create_get_node/1
        , delete_node/1
        , set_get_node_properties/1
        , set_get_node_property/1
        , delete_node_properties/1
        , delete_node_property/1
        , add_get_node_labels/1
        , set_node_labels/1
        , delete_node_label/1
        ]).

%% Relationships
-export([ create_get_relationship/1
        , create_get_relationship_props/1
        , delete_relationship/1
        , get_relationships/1
        , get_typed_relationships/1
        , set_get_relationship_properties/1
        , set_get_relationship_property/1
        , delete_relationship_properties/1
        , delete_relationship_property/1
        ]).

%% Indices
-export([ create_list_index/1
        , drop_index/1
        ]).

%% Constraints
-export([ create_get_constraint/1
        , drop_constraint/1
        ]).

%% Traverse
-export([ traverse/1
        , paged_traverse/1
        ]).

%% Graph algorithms
-export([ graph_algorithms/1
        ]).

%% Batch
-export([ batch/1
        ]).

%% Cypher
-export([ cypher/1
        ]).

%% Transactions
-export([ transactions/1
        ]).

%%_* Includes ==================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%_* Defines ==================================================================
-define(BASE_URI, <<"http://localhost:7474/db/data/">>).


%%_* CT Callbacks ==============================================================
all() ->
  [Name || {Name, _} <- ?MODULE:module_info(exports)
                , Name =/= all
                , Name =/= init_per_testcase
                , Name =/= module_info
                , Name =/= test].

init_per_testcase(connect, Config) ->
  Config;
init_per_testcase(_, Config) ->
  Neo = neo4j:connect([{base_uri, ?BASE_URI}]),
  [{neo, Neo} | Config].

%%_* Test cases ================================================================
%% -----------------------------------------------------------------------------
connect(_) ->
  Success200 = neo4j:connect([{base_uri, ?BASE_URI}]),
  Non200 = neo4j:connect([{base_uri, <<"http://localhost:7474/db/data">>}]),
  NoConnection = neo4j:connect([{base_uri, <<"http://localhost:747/db/data/">>}]),

  ?assertMatch({[_|_]}, Success200),
  ?assertMatch({error, {non_200_response, 302, <<>>}}, Non200),
  ?assertMatch({error, econnrefused}, NoConnection).

%% -----------------------------------------------------------------------------
create_node(Config) ->
  Node = neo4j:create_node(lkup(neo, Config)),
  ?assertMatch({[_|_]}, Node).

%% -----------------------------------------------------------------------------
%%
%% 1. Create a node with properties (properties are valid JSON strings)
%% 2. Retrieve this node
%%    Make sure that all properties you set are there in the retrieved node
%% 3. Retrieve node properties
%%    Make sure that all properties you set are there in the retrieved properties
%%
create_get_node(Config) ->
  Neo = lkup(neo, Config),

  Props = properties(),
  Node = neo4j:create_node(Neo, {Props}),

  RetrievedNode = neo4j:get_node(Neo, lkup(<<"self">>, Node)),
  RetrievedNodeProps = lkup(<<"data">>, RetrievedNode),
  RetrievedNodeCheck = lists:all(check(RetrievedNodeProps), Props),
  ?assert(RetrievedNodeCheck),

  RetrievedProps = neo4j:get_node_properties(Node),
  RetrievedPropsCheck = lists:all(check(RetrievedProps), Props),
  ?assert(RetrievedPropsCheck).

%% -----------------------------------------------------------------------------
delete_node(Config) ->
  Neo = lkup(neo, Config),
  Node = neo4j:create_node(Neo),

  Delete = neo4j:delete_node(Node),
  ?assertEqual(ok, Delete),

  Attempt = neo4j:get_node(Neo, lkup(<<"self">>, Node)),
  ?assertMatch({error, not_found}, Attempt),

  AttemptDelete = neo4j:delete_node(Node),
  ?assertMatch({error, {not_found, _}}, AttemptDelete).

%% -----------------------------------------------------------------------------
%%
%% 1. Create a node
%% 2. Set some properties
%% 3. Retrieve this node
%%    Make sure that all properties you set are there in the retrieved node
%% 4. Retrieve node properties
%%    Make sure that all properties you set are there in the retrieved properties
%%
set_get_node_properties(Config) ->
  Neo = lkup(neo, Config),

  Props = properties(),
  Node = neo4j:create_node(Neo),
  neo4j:set_node_properties(Node, {Props}),

  RetrievedNode = neo4j:get_node(Neo, lkup(<<"self">>, Node)),
  {RetrievedNodeProps} = lkup(<<"data">>, RetrievedNode),
  RetrievedNodeCheck = lists:all(check(RetrievedNodeProps), Props),
  ?assert(RetrievedNodeCheck),

  RetrievedProps = neo4j:get_node_properties(Node),
  RetrievedPropsCheck = lists:all(check(RetrievedProps), Props),
  ?assert(RetrievedPropsCheck).

%% -----------------------------------------------------------------------------
%%
%% 1. Create a node with properties
%% 2. Delete properties
%% 3. Retrieve this node
%%    Make sure that properties are empty
%% 4. Retrieve node properties
%%    Make sure that properties are empty
%% 5. Retrieve a property we just set
%%    Should be not_found
%%
delete_node_properties(Config) ->
  Neo = lkup(neo, Config),

  [{Key, _} | _] = Props = properties(),
  Node = neo4j:create_node(Neo, {Props}),
  neo4j:delete_node_properties(Node),

  RetrievedNode = neo4j:get_node(Neo, lkup(<<"self">>, Node)),
  {RetrievedNodeProps} = lkup(<<"data">>, RetrievedNode),
  ?assertMatch([], RetrievedNodeProps),

  RetrievedProps = neo4j:get_node_properties(Node),
  ?assertMatch({[]}, RetrievedProps),

  RetrievedProp = neo4j:get_node_property(Node, encode(Key)),
  ?assertMatch({error, not_found}, RetrievedProp).

%% -----------------------------------------------------------------------------
%%
%% 1. Create a node with properties
%% 2. Delete a property
%% 3. Retrieve node
%%    Make sure that the property isn't there
%% 4. Retrieve node properties
%%    Make sure that the property isn't there
%% 5. Retrieve the property
%%    It should be not_found
%%
delete_node_property(Config) ->
  Neo = lkup(neo, Config),

  [{Key, _}| _] = Props = properties(),
  Node = neo4j:create_node(Neo, {Props}),

  neo4j:delete_node_property(Node, Key),

  RetrievedNode = neo4j:get_node(Neo, lkup(<<"self">>, Node)),
  {RetrievedNodeProps} = lkup(<<"data">>, RetrievedNode),
  ?assertNot(lists:keyfind(Key, 1, RetrievedNodeProps)),

  {RetrievedProps} = neo4j:get_node_properties(Node),
  ?assertNot(lists:keyfind(Key, 1, RetrievedProps)),

  RetrievedProp = neo4j:get_node_property(Node, Key),
  ?assertMatch({error, not_found}, RetrievedProp).

%% -----------------------------------------------------------------------------
%%
%% 1. Create a node with properties
%% 2. Retrieve this node
%%    Make sure that all properties you set are there in the retrieved node
%% 3. Retrieve node properties
%%    Make sure that all properties you set are there in the retrieved properties
%% 4. Retrieve the property we just set
%%    Should exist and be equal to the data we provided
%%
set_get_node_property(Config) ->
  Neo = lkup(neo, Config),

  {Key, Val} = property(),
  Node = neo4j:create_node(Neo),
  neo4j:set_node_property(Node, Key, Val),

  RetrievedNode = neo4j:get_node(Neo, lkup(<<"self">>, Node)),
  {RetrievedNodeProps} = lkup(<<"data">>, RetrievedNode),
  ?assert(lists:keyfind(Key, 1, RetrievedNodeProps) == {Key, Val}),

  {RetrievedProps} = neo4j:get_node_properties(Node),
  ?assert(lists:keyfind(Key, 1, RetrievedProps) == {Key, Val}),

  RetrievedProp = neo4j:get_node_property(Node, Key),
  ?assert(RetrievedProp == Val).

%% -----------------------------------------------------------------------------
%%
%% 1. Create a node
%% 2. Add a single label
%% 3. Add a list of labels
%% 4. Retrieve node labels
%%    Each label should be in the list of labels
%%
add_get_node_labels(Config) ->
  Neo = lkup(neo, Config),

  Node = neo4j:create_node(Neo),

  [Label | OtherLabels] = Labels  = list_of_unicode_strings(2),
  neo4j:add_node_labels(Node, Label),
  neo4j:add_node_labels(Node, OtherLabels),

  RetrievedLabels = neo4j:get_node_labels(Node),
  [?assert(lists:member(Label, RetrievedLabels)) || Label <- Labels].

%% -----------------------------------------------------------------------------
%%
%% 1. Create a node
%% 2. Add several labels
%% 3. Set a different list of labels
%% 4. Retrieve node labels
%%    The list should only contain labels set in step 3
%%
set_node_labels(Config) ->
  Neo = lkup(neo, Config),

  Node = neo4j:create_node(Neo),

  OriginalLabels  = list_of_unicode_strings(3),
  NewLabels  = list_of_unicode_strings(3),
  neo4j:add_node_labels(Node, OriginalLabels),
  neo4j:set_node_labels(Node, NewLabels),

  RetrievedLabels = neo4j:get_node_labels(Node),
  ?assert(NewLabels -- RetrievedLabels == []).

%% -----------------------------------------------------------------------------
%%
%% 1. Create a node
%% 2. Add several labels
%% 3. Delete a label
%% 5. Retrieve node labels
%%    The list should not contain the deleted label
%%
delete_node_label(Config) ->
  Neo = lkup(neo, Config),

  Node = neo4j:create_node(Neo),

  [Label | _] = OriginalLabels  = list_of_unicode_strings(3),
  neo4j:add_node_labels(Node, OriginalLabels),
  neo4j:delete_node_label(Node, Label),

  RetrievedLabels = neo4j:get_node_labels(Node),
  ?assertNot(lists:member(Label, RetrievedLabels)).

%% -----------------------------------------------------------------------------
%%
%% 1. Create two nodes
%% 2. Create a relationship between them
%% 3. Retrieve the relationship
%%    It should be between the two nodes
%%
create_get_relationship(Config) ->
  Neo = lkup(neo, Config),

  Node1 = neo4j:create_node(Neo),
  Node2 = neo4j:create_node(Neo),

  Relationship = neo4j:create_relationship( Node1
                                          , Node2
                                          , <<"create_get_relationship">>),

  RetrievedRelationship = neo4j:get_relationship( Neo
                                                , lkup(<<"self">>, Relationship)
                                                ),
  ?assertEqual( lkup(<<"start">>, RetrievedRelationship)
              , lkup(<<"self">>, Node1)),
  ?assertEqual( lkup(<<"end">>, RetrievedRelationship)
              , lkup(<<"self">>, Node2)).

%% -----------------------------------------------------------------------------
%%
%% 1. Create two nodes
%% 2. Create a relationship between them with properties
%% 3. Retrieve the relationship
%%    It should be between the two nodes
%%    It should contain the set props
%% 4. Retrieve the relationship properties
%%    They should contain the set props
%%
create_get_relationship_props(Config) ->
  Neo = lkup(neo, Config),

  Node1 = neo4j:create_node(Neo),
  Node2 = neo4j:create_node(Neo),

  Props = properties(),
  Relationship = neo4j:create_relationship( Node1
                                          , Node2
                                          , <<"create_get_relationship">>
                                          , {Props}),

  RetrievedRelationship = neo4j:get_relationship( Neo
                                                , lkup(<<"self">>, Relationship)
                                                ),
  ?assertEqual( lkup(<<"start">>, RetrievedRelationship)
              , lkup(<<"self">>, Node1)),
  ?assertEqual( lkup(<<"end">>, RetrievedRelationship)
              , lkup(<<"self">>, Node2)),

  {RetrievedProps} = lkup(<<"data">>, RetrievedRelationship),
  [?assert(lists:member(Prop, Props)) || Prop <- RetrievedProps],

  {RetrievedRelationshipProps} = neo4j:get_relationship_properties(Relationship),
  [?assert(lists:member(Prop, Props)) || Prop <- RetrievedRelationshipProps].


%% -----------------------------------------------------------------------------
%%
%% 1. Create two nodes
%% 2. Create a relationship between them with properties
%% 3. Delete the relationship
%% 4. Retrieve the relationship
%%    Should be not_found
%%
delete_relationship(Config) ->
  Neo = lkup(neo, Config),

  Node1 = neo4j:create_node(Neo),
  Node2 = neo4j:create_node(Neo),

  Relationship = neo4j:create_relationship( Node1
                                          , Node2
                                          , <<"get_relationships">>),
  neo4j:delete_relationship(Relationship),

  RetrievedRelationship = neo4j:get_relationship( Neo
                                                , lkup(<<"self">>, Relationship)
                                                ),
  ?assertMatch({error, not_found}, RetrievedRelationship).

%% -----------------------------------------------------------------------------
%%
%% 1. Create two nodes, N1 and N2
%% 2. Create a relationship R1: N1 -> N2
%% 3. Create a relationship R2: N2 -> N1
%% 4. Retrieve `out` relationships on N1
%%    Should contain R1
%% 5. Retrieve `in` relationships on N1
%%    Should contain R2
%% 5. Retrieve `all` relationships on N1
%%    Should contain R1 and R2
%%
get_relationships(Config) ->
  Neo = lkup(neo, Config),

  Node1 = neo4j:create_node(Neo),
  Node2 = neo4j:create_node(Neo),

  _Relationship1 = neo4j:create_relationship( Node1
                                            , Node2
                                            , <<"get_relationships">>),

  _Relationship2 = neo4j:create_relationship( Node2
                                            , Node1
                                            , <<"get_relationships">>),

  [{RetrievedRelationship1}] = neo4j:get_relationships(Node1, out),
  ?assertEqual( lkup(<<"start">>, RetrievedRelationship1)
              , lkup(<<"self">>, Node1)),
  ?assertEqual( lkup(<<"end">>, RetrievedRelationship1)
              , lkup(<<"self">>, Node2)),

  [{RetrievedRelationship2}] = neo4j:get_relationships(Node1, in),
  ?assertEqual( lkup(<<"start">>, RetrievedRelationship2)
              , lkup(<<"self">>, Node2)),
  ?assertEqual( lkup(<<"end">>, RetrievedRelationship2)
              , lkup(<<"self">>, Node1)),

  RetrievedRelationship3 = neo4j:get_relationships(Node1, all),
  ?assertEqual(2, length(RetrievedRelationship3)).

%% -----------------------------------------------------------------------------
%%
%% 1. Create two nodes, N1 and N2
%% 2. Create a relationship R1: N1 -> N2 of type T
%% 3. Create a relationship R2: N2 -> N1 of type T
%% 4. Retrieve typed relationships of type T on node N1
%%    Should contain R1 and R2
%% 5. Retrieve `out` relationships of type T on N1
%%    Should contain R1
%% 5. Retrieve `in` relationships of type T on N1
%%    Should contain R2
%% 5. Retrieve `all` relationships of type T on N1
%%    Should contain R1 and R2
%%
get_typed_relationships(Config) ->
  Neo = lkup(neo, Config),

  Node1 = neo4j:create_node(Neo),
  Node2 = neo4j:create_node(Neo),

  _Relationship1 = neo4j:create_relationship( Node1
                                            , Node2
                                            , <<"get_typed_relationships">>),

  _Relationship2 = neo4j:create_relationship( Node2
                                            , Node1
                                            , <<"get_typed_relationships">>),

  RetrievedRelationship1 =
    neo4j:get_typed_relationships(Node1, <<"get_typed_relationships">>),
  ?assertEqual(2, length(RetrievedRelationship1)),

  [{RetrievedRelationship2}] =
    neo4j:get_typed_relationships(Node1, <<"get_typed_relationships">>, out),
  ?assertEqual( lkup(<<"start">>, RetrievedRelationship2)
              , lkup(<<"self">>, Node1)),
  ?assertEqual( lkup(<<"end">>, RetrievedRelationship2)
              , lkup(<<"self">>, Node2)),

  [{RetrievedRelationship3}] =
    neo4j:get_typed_relationships(Node1, <<"get_typed_relationships">>, in),
  ?assertEqual( lkup(<<"start">>, RetrievedRelationship3)
              , lkup(<<"self">>, Node2)),
  ?assertEqual( lkup(<<"end">>, RetrievedRelationship3)
              , lkup(<<"self">>, Node1)),

  RetrievedRelationship4 =
    neo4j:get_typed_relationships(Node1, <<"get_typed_relationships">>, all),
  ?assertEqual(2, length(RetrievedRelationship4)).

%% -----------------------------------------------------------------------------
%%
%% 1. Create two nodes
%% 2. Create a relationship between them
%% 3. Set properties on the relationship
%% 4. Retrieve relationship
%%    Make sure that all properties you set are there
%% 4. Retrieve relationship properties
%%    Make sure that all properties you set are there
%%
set_get_relationship_properties(Config) ->
  Neo = lkup(neo, Config),

  Props = properties(),
  Node1 = neo4j:create_node(Neo),
  Node2 = neo4j:create_node(Neo),
  Relationship = neo4j:create_relationship( Node1
                                          , Node2
                                          , <<"set_get_relationship_properties">>),
  neo4j:set_relationship_properties(Relationship, {Props}),

  RetrievedRelationship = neo4j:get_relationship( Neo
                                                , lkup(<<"self">>, Relationship)
                                                ),
  {Data} = lkup(<<"data">>, RetrievedRelationship),
  [?assert(lists:member(Prop, Props)) || Prop <- Data],

  {RetrievedRelationshipProps} = neo4j:get_relationship_properties(Relationship),
  [?assert(lists:member(Prop, Props)) || Prop <- RetrievedRelationshipProps].

%% -----------------------------------------------------------------------------
%%
%% 1. Create two nodes
%% 2. Create a relationship between them
%% 3. Set a property on the relationship
%% 4. Retrieve relationship
%%    Make sure that the property you set is there
%% 5. Retrieve relationship properties
%%    Make sure that the property you set is there
%% 5. Retrieve the property
%%    Make sure that the value is set
%%
set_get_relationship_property(Config) ->
  Neo = lkup(neo, Config),

  {Key, Value} = property(),
  Node1 = neo4j:create_node(Neo),
  Node2 = neo4j:create_node(Neo),
  Relationship = neo4j:create_relationship( Node1
                                          , Node2
                                          , <<"set_get_relationship_properties">>),
  neo4j:set_relationship_property(Relationship, Key, Value),

  RetrievedRelationship = neo4j:get_relationship( Neo
                                                , lkup(<<"self">>, Relationship)
                                                ),
  {Data} = lkup(<<"data">>, RetrievedRelationship),
  ?assert(lists:member({Key, Value}, Data)),

  {RetrievedRelationshipProps} = neo4j:get_relationship_properties(Relationship),
  ?assert(lists:member({Key, Value}, RetrievedRelationshipProps)),

  RetrievedProp = neo4j:get_relationship_property(Relationship, Key),
  ?assertEqual(Value, RetrievedProp).

%% -----------------------------------------------------------------------------
%%
%% 1. Create two nodes
%% 2. Create a relationship between them, with properties
%% 3. Delete relationship properties
%% 4. Retrieve relationship
%%    Make sure that properties are deleted
%% 5. Retrieve relationship properties
%%    Make sure that properties are deleted
%%
delete_relationship_properties(Config) ->
  Neo = lkup(neo, Config),

  Props = properties(),
  Node1 = neo4j:create_node(Neo),
  Node2 = neo4j:create_node(Neo),
  Relationship = neo4j:create_relationship( Node1
                                          , Node2
                                          , <<"set_get_relationship_properties">>
                                          , {Props}),
  neo4j:delete_relationship_properties(Relationship),

  RetrievedRelationship = neo4j:get_relationship( Neo
                                                , lkup(<<"self">>, Relationship)
                                                ),
  {Data} = lkup(<<"data">>, RetrievedRelationship),
  ?assertEqual([], Data),

  RetrievedRelationshipProps = neo4j:get_relationship_properties(Relationship),
  ?assertEqual({[]}, RetrievedRelationshipProps).

%% -----------------------------------------------------------------------------
%%
%% 1. Create two nodes
%% 2. Create a relationship between them, with properties
%% 3. Delete a relationship properties
%% 4. Retrieve relationship
%%    Make sure that the deleted property isn't there
%% 5. Retrieve relationship properties
%%    Make sure that the deleted property isn't there
%% 5. Retrieve the property
%%    Should be not_found
%%
delete_relationship_property(Config) ->
  Neo = lkup(neo, Config),

  [{Key, _} = Prop | _] = Props = properties(),
  Node1 = neo4j:create_node(Neo),
  Node2 = neo4j:create_node(Neo),
  Relationship = neo4j:create_relationship( Node1
                                          , Node2
                                          , <<"delete_relationship_property">>
                                          , {Props}),
  neo4j:delete_relationship_property(Relationship, Key),

  RetrievedRelationship = neo4j:get_relationship( Neo
                                                , lkup(<<"self">>, Relationship)
                                                ),
  {Data} = lkup(<<"data">>, RetrievedRelationship),
  ?assertNot(lists:member(Prop, Data)),

  {RetrievedRelationshipProps} = neo4j:get_relationship_properties(Relationship),
  ?assertNot(lists:member(Prop, RetrievedRelationshipProps)),

  RetrievedProp = neo4j:get_relationship_property(Relationship, Key),
  ?assertEqual({error, not_found}, RetrievedProp).

%% -----------------------------------------------------------------------------
%%
%% 1. Create an index
%% 2. List indices
%%    Created index should be in the list of indices
%%
create_list_index(Config) ->
  Neo = lkup(neo, Config),

  Label = unicode_string(),
  PropKey = unicode_string(), %% compound indices are not yet supported in neo4j
  Index = neo4j:create_index(Neo, Label, [PropKey]),
  ?assertMatch({[_|_]}, Index),

  List = neo4j:list_indexes(Neo, Label),
  Compare = fun(I) ->
              lkup(<<"label">>, I) == lkup(<<"label">>, Index) andalso
              lkup(<<"property_keys">>, I) == lkup(<<"property_keys">>, Index)
            end,
  ?assert(lists:any(Compare, List)).

%% -----------------------------------------------------------------------------
%%
%% 1. Create an index
%% 2. Drop index
%% 2. List indices
%%    The index should not be there
%%
drop_index(Config) ->
  Neo = lkup(neo, Config),

  Label = unicode_string(),
  PropKey = unicode_string(), %% compound indices are not yet supported in neo4j
  Index = neo4j:create_index(Neo, Label, [PropKey]),
  neo4j:drop_index(Neo, Label, PropKey),

  List = neo4j:list_indexes(Neo, Label),
  Compare = fun(I) ->
              lkup(<<"label">>, I) == lkup(<<"label">>, Index) andalso
              lkup(<<"property_keys">>, I) == lkup(<<"property_keys">>, Index)
            end,
  ?assertNot(lists:any(Compare, List)).

%% -----------------------------------------------------------------------------
%%
%% 1. Create a constraint
%% 2. Get constraint
%% 3. Get label constraints
%%    The constraint should be there
%% 4. Get constraints
%%    The constraint should be there
%%
create_get_constraint(Config) ->
  Neo = lkup(neo, Config),

  Label = unicode_string(),
  PropKey = unicode_string(),
  Constraint = neo4j:create_constraint(Neo, Label, [PropKey]),

  Compare = fun(I) ->
    lkup(<<"label">>, I) == lkup(<<"label">>, Constraint) andalso
    lkup(<<"property_keys">>, I) == lkup(<<"property_keys">>, Constraint)
  end,

  RetrievedConstraint = neo4j:get_constraint(Neo, Label, PropKey),
  case RetrievedConstraint of
    {error, not_found} ->
      ct:pal("Constraint failed on ~nLabel ~p~nPropKey ~p~n", [Label, PropKey]),
      ?assert(false);
    _ ->
      ?assert(lists:any(Compare, RetrievedConstraint))
  end,

  RetrievedLabelConstraint = neo4j:get_label_constraints(Neo, Label),
  ?assert(lists:any(Compare, RetrievedLabelConstraint)),

  RetrievedConstraints = neo4j:get_constraints(Neo),
  ?assert(lists:any(Compare, RetrievedConstraints)).

%% -----------------------------------------------------------------------------
%%
%% 1. Create a constraint
%% 2. Drop constraint
%% 2. Get constraint
%%    Should be non_existent
%% 3. Get label constraints
%%    The constraint should not be there
%% 4. Get constraints
%%    The constraint should not be there
%%
drop_constraint(Config) ->
  Neo = lkup(neo, Config),

  Label = unicode_string(),
  PropKey = unicode_string(),
  Constraint = neo4j:create_constraint(Neo, Label, [PropKey]),
  neo4j:drop_constraint(Neo, Label, PropKey),

  Compare = fun(I) ->
    lkup(<<"label">>, I) == lkup(<<"label">>, Constraint) andalso
    lkup(<<"property_keys">>, I) == lkup(<<"property_keys">>, Constraint)
  end,

  RetrievedConstraint = neo4j:get_constraint(Neo, Label, PropKey),
  ?assertEqual({error, not_found}, RetrievedConstraint),

  RetrievedLabelConstraint = neo4j:get_label_constraints(Neo, Label),
  ?assertNot(lists:any(Compare, RetrievedLabelConstraint)),

  RetrievedConstraints = neo4j:get_constraints(Neo),
  ?assertNot(lists:any(Compare, RetrievedConstraints)).

%% -----------------------------------------------------------------------------
%%
%% 1. Create a node
%% 2. Start a traverse
%%
traverse(Config) ->
  Neo = lkup(neo, Config),

  Node = neo4j:create_node(Neo),
  Body = {[ {<<"order">>, <<"breadth_first">>}
          , {<<"uniqueness">>, <<"none">>}
          , {<<"return_filter">>, {[ {<<"language">>, <<"builtin">>}
                                   , {<<"name">>, <<"all">>}
                                   ]}
            }
          ]},
  Traverse1 = neo4j:traverse(Node, Body),
  ?assertMatch([{[_|_]}], Traverse1),

  Traverse2 = neo4j:traverse(Node, Body, <<"fullpath">>),
  ?assertMatch([{[_|_]}], Traverse2).

%% -----------------------------------------------------------------------------
%%
%% 1. Create a node
%% 2. Start a paged traverse
%% 3. Traverse until not_found
%%
paged_traverse(Config) ->
  Neo = lkup(neo, Config),

  Node = neo4j:create_node(Neo),
  Body = {[ {<<"order">>, <<"breadth_first">>}
          , {<<"uniqueness">>, <<"none">>}
          , {<<"return_filter">>, {[ {<<"language">>, <<"builtin">>}
                                   , {<<"name">>, <<"all">>}
                                   ]}
            }
          ]},
  Options = [ {<<"returnType">>, <<"fullpath">>}
            , {<<"leaseTime">>, <<"1000">>}
            , {<<"pageSize">>, <<"10">>}
            ],
  Traverse1 = neo4j:paged_traverse(Node, Body),
  ?assertMatch([_, {[_|_]} | _], Traverse1),
  Traverse2 = neo4j:paged_traverse(Traverse1),
  ?assertEqual({error, not_found}, Traverse2),

  Traverse3 = neo4j:paged_traverse(Node, Body, Options),
  ?assertMatch([_, {[_ | _]} | _], Traverse3),
  Traverse4 = neo4j:paged_traverse(Traverse3),
  ?assertEqual({error, not_found}, Traverse4).


%% -----------------------------------------------------------------------------
%% 1. Create two nodes
%% 2. Create a relationship between them
%% 3. Apply algorithms
%%
graph_algorithms(Config) ->
  Neo = lkup(neo, Config),

  Node1 = neo4j:create_node(Neo),
  Node2 = neo4j:create_node(Neo),
  neo4j:create_relationship(Node1, Node2, <<"graph_algorithms">>),
  AdditionalParams = [{<<"relationships">>, {[ {<<"type">>, <<"to">>}
                                             , {<<"direction">>, <<"out">>}
                                             ]}
                      }
                     ],

  Paths = neo4j:paths(Node1, <<"shortestPath">>, 3, AdditionalParams),
  ?assertMatch([{[_|_]}], Paths),

  Path = neo4j:path(Node1, <<"shortestPath">>, 3, AdditionalParams),
  ?assertMatch({[_|_]}, Path),

  GraphAlgo = neo4j:graph_algorithm( <<"path">>
                                   , Node1
                                   , <<"shortestPath">>
                                   , 3
                                   , AdditionalParams),
  ?assertMatch({[_|_]}, GraphAlgo).

%% -----------------------------------------------------------------------------
%% 1. Perform a batch operation
%%
batch(Config) ->
  Neo = lkup(neo, Config),

  Node = neo4j:create_node(Neo),

  URI = lkup(<<"self">>, Node),
  ID = lists:last(binary:split(URI, <<"/">>, [global])),

  Payload = [ {[ {<<"method">>, <<"PUT">>}
               , {<<"to">>, <<"/node/", ID/binary, "/properties">>}
               , {<<"body">>, {[{<<"age">>, 1}]}}
               , {<<"id">>, 0}
               ]}
            ,  {[ {<<"method">>, <<"GET">>}
                , {<<"to">>, <<"/node/", ID/binary>>}
                , {<<"id">>, 1}
                ]}
            ,  {[ {<<"method">>, <<"POST">>}
                , {<<"to">>, <<"/node">>}
                , {<<"body">>, {[{<<"age">>, 1}]}}
                , {<<"id">>, 2}
                ]}
            ,  {[ {<<"method">>, <<"POST">>}
                , {<<"to">>, <<"/node">>}
                , {<<"body">>, {[{<<"age">>, 1}]}}
                , {<<"id">>, 3}
                ]}
            ],

  Result = neo4j:batch(Neo, Payload),
  ?assertMatch([_|_], Result),

  RetrievedProperties = neo4j:get_node_properties(Node),
  ?assertEqual(1, lkup(<<"age">>, RetrievedProperties)).

%% -----------------------------------------------------------------------------
%% 1. Check that we send in Cypher queries correctly
%%
cypher(Config) ->
  Neo = lkup(neo, Config),

  Cypher = <<"CREATE (n:Person {name: 'this property is to be deleted'}) SET n = {props} RETURN n">>,
  Props = {[{<<"props">>, {[ {<<"position">>,  <<"Developer">>}
                          , {<<"firstName">>, <<"Michael">>}
                          , {<<"awesome">>, true}
                          , {<<"children">>, 3}
                          ]}}
           ]},

  Result = neo4j:cypher(Neo, Cypher, Props),
  ?assertMatch({[{<<"columns">>, _}, {<<"data">>, _}]}, Result).

%% -----------------------------------------------------------------------------
%% 1. Check that we send transaction requests correctly
%%
transactions(Config) ->
  Neo = lkup(neo, Config),

  Query = [ { <<"CREATE (n {props}) RETURN n">>
            , {[{<<"props">>, {[{ <<"prop">>, <<"My Node">>}]}}]}
            , [<<"REST">>] %% optional parameter for data format
            }
          ],

  T1 = neo4j:transaction_begin(Neo, Query),
  ?assertMatch({[_|_]}, T1),

  T1_1 = neo4j:transaction_execute(T1, Query),
  ?assertMatch({[_|_]}, T1_1),

  T1_2 = neo4j:transaction_commit(T1),
  ?assertMatch({[_ | _]}, T1_2),

  T2 = neo4j:transaction_begin(Neo, Query),
  ?assertMatch({[_|_]}, T2),

  T2_1 = neo4j:transaction_execute(T2, Query),
  ?assertMatch({[_|_]}, T2_1),

  T2_2 = neo4j:transaction_rollback(T2),
  ?assertMatch({[_|_]}, T2_2),

  T3 = neo4j:transaction_execute_commit(Neo, Query),
  ?assertMatch({[_|_]}, T3).


%%_* Helper functions ==========================================================

lkup(What, {List}) ->
  lkup(What, List);
lkup(What, List) ->
  proplists:get_value(What, List).

unicode_char() ->
  case crypto:rand_uniform(0, 3) of
    0 -> crypto:rand_uniform(0, 16#ff);
    1 -> crypto:rand_uniform(16#300, 16#2000);
    2 -> crypto:rand_uniform(16#20d0, 16#2100)
  end.

unicode_string() ->
  Length = crypto:rand_uniform(1, 101),
  String = [unicode_char() || _ <- lists:seq(1, Length)],
  unicode:characters_to_binary(String).

property() ->
  {unicode_string(), unicode_string()}.

properties() ->
  Length = crypto:rand_uniform(1, 11),
  [property() || _ <- lists:seq(1, Length)].

list_of_unicode_strings(AtLeast) ->
  Length = rand(AtLeast),
  [unicode_string() || _ <- lists:seq(1, Length)].


rand(AtLeast) ->
  case crypto:rand_uniform(1, 10 + AtLeast) of
    N when N < AtLeast -> rand(AtLeast);
    Rand -> Rand
  end.

check(Where) ->
  fun({Property, Value}) ->
    lkup(Property, Where) == Value
  end.

%% https://gist.github.com/renatoalbano/3796470
%% Taken from <http://erlangexamples.com/>,
%% from <http://github.com/CapnKernul/httparadise>
%% and <http://www.erlang.org/doc/man/edoc_lib.html>

encode(B) when is_binary(B) ->
  unicode:characters_to_binary(encode(unicode:characters_to_list(B)));
encode([C | Cs]) when C >= $a, C =< $z ->
  [C | encode(Cs)];
encode([C | Cs]) when C >= $A, C =< $Z ->
  [C | encode(Cs)];
encode([C | Cs]) when C >= $0, C =< $9 ->
  [C | encode(Cs)];

encode([C | Cs]) when C == 16#20 -> % space to +
  [$+ | encode(Cs)];

% unreserved
encode([C = $- | Cs]) ->
  [C | encode(Cs)];
encode([C = $_ | Cs]) ->
  [C | encode(Cs)];
encode([C = 46 | Cs]) -> % .
  [C | encode(Cs)];
encode([C = $! | Cs]) ->
  [C | encode(Cs)];
encode([C = $~ | Cs]) ->
  [C | encode(Cs)];
encode([C = $* | Cs]) ->
  [C | encode(Cs)];
encode([C = 39 | Cs]) -> % '
  [C | encode(Cs)];
encode([C = $( | Cs]) ->
  [C | encode(Cs)];
encode([C = $) | Cs]) ->
  [C | encode(Cs)];

encode([C | Cs]) when C =< 16#7f ->
  escape_byte(C)
  ++ encode(Cs);

encode([C | Cs]) when (C >= 16#7f) and (C =< 16#07FF) ->
  escape_byte((C bsr 6) + 16#c0)
  ++ escape_byte(C band 16#3f + 16#80)
     ++ encode(Cs);

encode([C | Cs]) when (C > 16#07FF) ->
  escape_byte((C bsr 12) + 16#e0) % (0xe0 | C >> 12)
  ++ escape_byte((16#3f band (C bsr 6)) + 16#80) % 0x80 | ((C >> 6) & 0x3f)
     ++ escape_byte(C band 16#3f + 16#80) % 0x80 | (C >> 0x3f)
        ++ encode(Cs);

encode([C | Cs]) ->
  escape_byte(C) ++ encode(Cs);

encode([]) -> [].

% from edoc_lib source
hex_octet(N) when N =< 9 ->
  [$0 + N];
hex_octet(N) when N > 15 ->
  hex_octet(N bsr 4) ++ hex_octet(N band 15);
hex_octet(N) ->
  [N - 10 + $a].

escape_byte(C) ->
  H = hex_octet(C),
  normalize(H).

%% Append 0 if length == 1
normalize(H) when length(H) == 1 ->
  "%0" ++ H;

normalize(H) ->
  "%" ++ H.
