# Erlang client library for Neo4J

This is a lightweight wrapper for [Neo4j REST API](http://docs.neo4j.org/chunked/stable/rest-api.html).

## What?

- Implements all of Neo4J 2.0.0's REST API as referenced [here](http://docs.neo4j.org/chunked/stable/rest-api.html) with one caveat:
 - Does *not* implement [streaming API](http://docs.neo4j.org/chunked/stable/rest-api-streaming.html)
- Uses [jsonx](https://github.com/iskra/jsonx) for extra fast JSON parsing, albeit in `{format, proplist}` mode
- Uses [hackney](https://github.com/benoitc/hackney) for http queries
 - Does *not* support HTTPS (yet?)

## How?

### Sample session

```erlang

Neo = neo4j:connect([{base_uri, <<"http://localhost:7474/db/data/">>}]),

StartNode = neo4j:get_node(Neo, 101),
EndNode = neo4j:create_node(Neo, [{<<"prop1">>, <<"key1">>}]),

Relationship1 = neo4:create_relationship(StartNode, EndNode, <<"KNOWS">>),
Relationship2 = neo4:create_relationship(StartNode, EndNode, <<"KNOWS">>, [{<<"prop2">>, <<"value2">>}]),

ok = neo4j:delete_relationship(Relationship1).

```

### Details

The wrapper follows [Neo4j's REST API](http://docs.neo4j.org/chunked/stable/rest-api.html) as close as possible. See code comments for direct links to each method/feature implemented. For example:

```erlang
%%
%% http://docs.neo4j.org/chunked/stable/rest-api-nodes.html#rest-api-create-node
%%
-spec create_node(neo4j_root()) -> neo4j_node() | {error, term()}.
create_node(Neo) ->
  {_, URI} = lists:keyfind(<<"node">>, 1, Neo),
  create(URI).
```

That link will tell you exactly what's going on and what you should expect.

### Errors

There are two types of errors the wrapper returns:
- `{error, atom()}` — some generic errors like `{error, not_found}` (which you can use for paged traversals):

```erlang
> neo4j:get_node(Neo, 10000).
{error,not_found}
```

- `{error, {integer(), proplists:proplist()}` — errors returned by Neo4j. Example of such an error (using [unique indexing](http://docs.neo4j.org/chunked/stable/rest-api-unique-indexes.html#rest-api-create-a-unique-node-or-return-fail-create)):

```erlang
> neo4j:unique_create_node(Neo, [{<<"prop">>, <<"val">>}], <<"index">>, <<"key">>, <<"value">>, <<"create_or_fail">>).
[{<<"self">>,
  <<"http://localhost:7474/db/data/index/node/index/key/value/191">>},
 {<<"extensions">>,[]},
 {<<"paged_traverse">>,
  <<"http://localhost:7474/db/data/node/191/paged/traverse/{returnType}{?pageSize,leaseTime}">>},
 {<<"labels">>,
...

> neo4j:unique_create_node(Neo, [{<<"prop">>, <<"val">>}], <<"index">>, <<"key">>, <<"value">>, <<"create_or_fail">>).

error,{409,
        <<"http://localhost:7474/db/data/index/node/index?uniqueness=create_or_fail">>,
        [{<<"extensions">>,[]},
         {<<"paged_traverse">>,
...
```

As [expected](http://docs.neo4j.org/chunked/stable/rest-api-unique-indexes.html#rest-api-create-a-unique-node-or-return-fail-fail), Neo4j returned a [HTTP 409 Conflict](https://github.com/for-GET/know-your-http-well/blob/master/status-codes.md) code.

## Assumptions

### No JSON, just proplists

Even for complex queries (such as [Cypher queries](http://docs.neo4j.org/chunked/stable/rest-api-cypher.html) or [transactions](http://docs.neo4j.org/chunked/stable/rest-api-transactional.html)) you never send in raw JSON, only proplists representing your objects:

See "Binaries" below

### Binaries

All string data and all URL parameters sent to Neo4J are assumed to be binaries.

As an example, let's create a [paged traverser](http://docs.neo4j.org/chunked/milestone/rest-api-traverse.html#rest-api-creating-a-paged-traverser)

```erlang
Neo = neo4j:connect([{base_uri, BaseUri}]),
Node = neo4j:get_node(Neo, 101),
Body = [ {<<"order">>, <<"breadth_first">>}
       , {<<"uniqueness">>, <<"none">>}
       , {<<"return_filter">>, [ {<<"language">>, <<"builtin">>}
                               , {<<"name">>, <<"all">>}
                               ]
         }
       ],
PT = neo4j:paged_traverse(Node, Body, [ {<<"returnType">>, ReturnType}
                                      , {<<"leaseTime">>, LeaseTime}
                                      , {<<"pageSize">>, PageSize}
                                      ]).
```

### Does *not* do stuff for you

- Will not urlencode your parameters (as required [here](http://docs.neo4j.org/chunked/stable/rest-api-indexes.html#rest-api-find-node-by-query)). You'll have to do it manually
- Will not assume that an integer/url references a valid node/relationship. You'll have to retrieve nodes/relationships yourself. Typical workflow looks something like this:

```erlang
Neo = neo4j:connect([{base_uri, <<"http://localhost:7474/db/data/">>}]),

%% will not work:
neo4j:get_node_properties(101).

%% will not work:
neo4j:get_node_properties(<<"http://localhost:7474/db/data/node/101">>).

%% correct way:
Node = neo4j:get_node(N, 101),
neo4j:get_node_properties(Node).

%% also correct:
Node2 = neo4j:get_node(N, <<"http://localhost:7474/db/data/node/101">>),
neo4j:get_node_properties(Node2).
```

## Contributing

Yes, please! :) If you have ideas, suggestions, pull requests or issues, do not hesitate to send them my way
