%%% @doc Provides client for Neo4j's REST API v1.5
-module(neo4j_api).

-compile({no_auto_import, [ error/1 ]}).

%%%_* Exports ==================================================================
%%%_* High Level API -----------------------------------------------------------

-export([ getNodeId/1
        , performCypherQuery/2
        , performGremlinScript/2
        , version/0
        ]).

%%%_* Low Level API ------------------------------------------------------------
-export([ addToNodeIndex/3
        , addToRelationshipIndex/3
        , allPaths/3
        , createNode/2
        , createPagedTraverser/4
        , createRelationship/3
        , deleteAllNodeProperties/2
        , deleteAllRelationshipProperties/2
        , deleteFromNodeIndex/5
        , deleteFromNodeIndexNoKeyValue/3
        , deleteFromNodeIndexNoValue/4
        , deleteFromRelationshipIndex/3
        , deleteFromRelationshipIndex/5
        , deleteFromRelationshipIndexNoValue/4
        , deleteNode/2
        , deleteNodeIndex/2
        , deleteNodeProperty/3
        , deleteRelationship/2
        , deleteRelationshipIndex/2
        , deleteRelationshipProperty/3
        , getAllNodeProperties/2
        , getAllRelationshipProperties/2
        , getAutoIndexedNodesByQuery/2
        , getAutoIndexedRelationshipsByQuery/2
        , getExtensionList/2
        , getExtensionsList/1
        , getGraphDatabaseExtensionDescription/3
        , getHtmlBrowseJavascript/1
        , getIndexedNodes/3
        , getIndexedNodes/4
        , getIndexedNodesByQuery/3
        , getIndexedNodesByQuery/4
        , getIndexedRelationships/3
        , getIndexedRelationships/4
        , getIndexedRelationshipsByQuery/3
        , getIndexedRelationshipsByQuery/4
        , getNode/2
        , getNodeExtensionDescription/4
        , getNodeFromIndexUri/5
        , getNodeIndexRoot/1
        , getNodeProperty/3
        , getNodeRelationships/3
        , getNodeRelationships/4
        , getRelationship/2
        , getRelationshipExtensionDescription/4
        , getRelationshipFromIndexUri/5
        , getRelationshipIndexRoot/1
        , getRelationshipProperty/3
        , getRelationshipTypes/1
        , getRoot/1
        , invokeGraphDatabaseExtension/4
        , invokeNodeExtension/5
        , invokeRelationshipExtension/5
        , jsonCreateNodeIndex/2
        , jsonCreateRelationshipIndex/2
        , pagedTraverse/4
        , performBatchOperations/2
        , removePagedTraverser/4
        , setAllNodeProperties/3
        , setAllRelationshipProperties/3
        , setNodeProperty/4
        , setRelationshipProperty/4
        , singlePath/3
        , traverse/4
        ]).

%%%_* Code =====================================================================
%%%_* High Level API ---------------------------------------------------
getNodeId(Node) ->
  try
    NodeURI = erlang:binary_to_list(kf(b("self"), Node)),
    NodeId = erlang:list_to_integer(filename:basename(NodeURI)),
    {ok, NodeId}
  catch _:_ -> error(undefined)
  end.

performCypherQuery(BaseUri, Query) ->
  invokeGraphDatabaseExtension(BaseUri, "CypherPlugin", "execute_query", Query).

performGremlinScript(BaseUri, Script) ->
  invokeGraphDatabaseExtension(BaseUri, "GremlinPlugin", "execute_script", Script).

version() ->
  "1.5".

%%%_* Low Level API ----------------------------------------------------
addToNodeIndex(BaseUri, PindexName, Data) ->
  rest_client:request(post, j(BaseUri, ["index", "node", PindexName]), Data).

addToRelationshipIndex(BaseUri, PindexName, Data) ->
  rest_client:request( post, j(BaseUri, ["index","relationship",PindexName]), Data).

allPaths(BaseUri, PnodeId, Data) ->
  rest_client:request(post, j(BaseUri, ["node",PnodeId,"paths"]), Data).

createNode(BaseUri, Data) ->
  rest_client:request(post, j(BaseUri, ["node"]), Data).

createPagedTraverser(BaseUri, PnodeId, PreturnType, Data) ->
  rest_client:request(post, j(BaseUri, ["node",PnodeId,"paged","traverse",PreturnType]), Data).

createRelationship(BaseUri, PnodeId, Data) ->
  rest_client:request(post, j(BaseUri, ["node",PnodeId,"relationships"]), Data).

deleteAllNodeProperties(BaseUri, PnodeId) ->
  rest_client:request(delete, j(BaseUri, ["node",PnodeId,"properties"])).

deleteAllRelationshipProperties(BaseUri, PrelationshipId) ->
  rest_client:request(delete, j(BaseUri, ["relationship",PrelationshipId,"properties"])).

deleteFromNodeIndex(BaseUri, Pvalue, PindexName, Pkey, Pid) ->
  rest_client:request(delete, j(BaseUri, ["index","node",PindexName,Pkey,Pvalue,Pid])).

deleteFromNodeIndexNoKeyValue(BaseUri, PindexName, Pid) ->
  rest_client:request(delete, j(BaseUri, ["index","node",PindexName,Pid])).

deleteFromNodeIndexNoValue(BaseUri, PindexName, Pkey, Pid) ->
  rest_client:request(delete, j(BaseUri, ["index","node",PindexName,Pkey,Pid])).

deleteFromRelationshipIndex(BaseUri, PindexName, Pid) ->
  rest_client:request(delete, j(BaseUri, ["index","relationship",PindexName,Pid])).

deleteFromRelationshipIndex(BaseUri, Pvalue, PindexName, Pkey, Pid) ->
  rest_client:request(delete, j(BaseUri, ["index","relationship",PindexName,Pkey,Pvalue,Pid])).

deleteFromRelationshipIndexNoValue(BaseUri, PindexName, Pkey, Pid) ->
  rest_client:request(delete, j(BaseUri, ["index","relationship",PindexName,Pkey,Pid])).

deleteNode(BaseUri, PnodeId) ->
  rest_client:request(delete, j(BaseUri, ["node",PnodeId])).

deleteNodeIndex(BaseUri, PindexName) ->
  rest_client:request(delete, j(BaseUri, ["index","node",PindexName])).

deleteNodeProperty(BaseUri, PnodeId, Pkey) ->
  rest_client:request(delete, j(BaseUri, ["node",PnodeId,"properties",Pkey])).

deleteRelationship(BaseUri, PrelationshipId) ->
  rest_client:request(delete, j(BaseUri, ["relationship",PrelationshipId])).

deleteRelationshipIndex(BaseUri, PindexName) ->
  rest_client:request(delete, j(BaseUri, ["index","relationship",PindexName])).

deleteRelationshipProperty(BaseUri, PrelationshipId, Pkey) ->
  rest_client:request(delete, j(BaseUri, ["relationship",PrelationshipId,"properties",Pkey])).

getAllNodeProperties(BaseUri, PnodeId) ->
  rest_client:request(get, j(BaseUri, ["node",PnodeId,"properties"])).

getAllRelationshipProperties(BaseUri, PrelationshipId) ->
  rest_client:request(get, j(BaseUri, ["relationship",PrelationshipId,"properties"])).

getAutoIndexedNodesByQuery(BaseUri, Data) ->
  rest_client:request(get, j(BaseUri, ["index","auto","node"]), Data).

getAutoIndexedRelationshipsByQuery(BaseUri, Data) ->
  rest_client:request(get, j(BaseUri, ["index","auto","relationship"]), Data).

getExtensionList(BaseUri, Pname) ->
  rest_client:request(get, j(BaseUri, ["ext",Pname])).

getExtensionsList(BaseUri) ->
  rest_client:request(get, j(BaseUri, ["ext"])).

getGraphDatabaseExtensionDescription(BaseUri, Pname, Pmethod) ->
  rest_client:request(get, j(BaseUri, ["ext",Pname,"graphdb",Pmethod])).

getHtmlBrowseJavascript(BaseUri) ->
  rest_client:request(get, j(BaseUri, ["resource","htmlbrowse.js"])).

getIndexedNodes(BaseUri, PindexName, Pkey, Pvalue) ->
  rest_client:request(get, j(BaseUri, ["index","node",PindexName,Pkey,Pvalue])).

getIndexedNodes(BaseUri, Pkey, Pvalue) ->
  rest_client:request(get, j(BaseUri, ["index","auto","node",Pkey,Pvalue])).

getIndexedNodesByQuery(BaseUri, PindexName, Data) ->
  rest_client:request(get, j(BaseUri, ["index","node",PindexName]), Data).

getIndexedNodesByQuery(BaseUri, PindexName, Pkey, Data) ->
  rest_client:request(get, j(BaseUri, ["index","node",PindexName,Pkey]), Data).

getIndexedRelationships(BaseUri, PindexName, Pkey, Pvalue) ->
  rest_client:request(get, j(BaseUri, ["index","relationship",PindexName,Pkey,Pvalue])).

getIndexedRelationships(BaseUri, Pkey, Pvalue) ->
  rest_client:request(get, j(BaseUri, ["index","auto","relationship",Pkey,Pvalue])).

getIndexedRelationshipsByQuery(BaseUri, PindexName, Data) ->
  rest_client:request(get, j(BaseUri, ["index","relationship",PindexName]), Data).

getIndexedRelationshipsByQuery(BaseUri, PindexName, Pkey, Data) ->
  rest_client:request(get, j(BaseUri, ["index","relationship",PindexName,Pkey]), Data).

getNode(BaseUri, PnodeId) ->
  rest_client:request(get, j(BaseUri, ["node",PnodeId])).

getNodeExtensionDescription(BaseUri, Pname, PnodeId, Pmethod) ->
  rest_client:request(get, j(BaseUri, ["ext",Pname,"node",PnodeId,Pmethod])).

getNodeFromIndexUri(BaseUri, PindexName, Pkey, Pvalue, Pid) ->
  rest_client:request(get, j(BaseUri, ["index","node",PindexName,Pkey,Pvalue,Pid])).

getNodeIndexRoot(BaseUri) ->
  rest_client:request(get, j(BaseUri, ["index","node"])).

getNodeProperty(BaseUri, PnodeId, Pkey) ->
  rest_client:request(get, j(BaseUri, ["node",PnodeId,"properties",Pkey])).

getNodeRelationships(BaseUri, PnodeId, Pdirection)
  when Pdirection =:= in orelse Pdirection =:= "in" orelse
       Pdirection =:= out orelse Pdirection =:= "out" ->
  rest_client:request(get, j(BaseUri, ["node",PnodeId,"relationships",Pdirection])).

getNodeRelationships(BaseUri, PnodeId, Pdirection, Ptypes)
  when Pdirection =:= in orelse Pdirection =:= "in" orelse
       Pdirection =:= out orelse Pdirection =:= "out" ->
  rest_client:request(get, j(BaseUri, ["node",PnodeId,"relationships",Pdirection,Ptypes])).

getRelationship(BaseUri, PrelationshipId) ->
  rest_client:request(get, j(BaseUri, ["relationship",PrelationshipId])).

getRelationshipExtensionDescription(BaseUri, Pname, PrelationshipId, Pmethod) ->
  rest_client:request(get, j(BaseUri, ["ext",Pname,"relationship",PrelationshipId,Pmethod])).

getRelationshipFromIndexUri(BaseUri, PindexName, Pkey, Pvalue, Pid) ->
  rest_client:request(get, j(BaseUri, ["index","relationship",PindexName,Pkey,Pvalue,Pid])).

getRelationshipIndexRoot(BaseUri) ->
  rest_client:request(get, j(BaseUri, ["index","relationship"])).

getRelationshipProperty(BaseUri, PrelationshipId, Pkey) ->
  rest_client:request(get, j(BaseUri, ["relationship",PrelationshipId,"properties",Pkey])).

getRelationshipTypes(BaseUri) ->
  rest_client:request(get, j(BaseUri, ["relationship","types"])).

getRoot(BaseUri) ->
  rest_client:request(get, j(BaseUri, [])).

invokeGraphDatabaseExtension(BaseUri, Pname, Pmethod, Data) ->
  rest_client:request(post, j(BaseUri, ["ext",Pname,"graphdb",Pmethod]), Data).

invokeNodeExtension(BaseUri, Pname, PnodeId, Pmethod, Data) ->
  rest_client:request(post, j(BaseUri, ["ext",Pname,"node",PnodeId,Pmethod]), Data).

invokeRelationshipExtension(BaseUri, Pname, PrelationshipId, Pmethod, Data) ->
  rest_client:request(post, j(BaseUri, ["ext",Pname,"relationship",PrelationshipId,Pmethod]), Data).

jsonCreateNodeIndex(BaseUri, Data) ->
  rest_client:request(post, j(BaseUri, ["index","node"]), Data).

jsonCreateRelationshipIndex(BaseUri, Data) ->
  rest_client:request(post, j(BaseUri, ["index","relationship"]), Data).

pagedTraverse(BaseUri, PnodeId, PreturnType, PtraverserId) ->
  rest_client:request(get, j(BaseUri, ["node",PnodeId,"paged","traverse",PreturnType,PtraverserId])).

performBatchOperations(BaseUri, Data) ->
  rest_client:request(post, j(BaseUri, ["batch"]), Data).

removePagedTraverser(BaseUri, PnodeId, PreturnType, PtraverserId) ->
  rest_client:request(delete, j(BaseUri, ["node",PnodeId,"paged","traverse",PreturnType,PtraverserId])).

setAllNodeProperties(BaseUri, PnodeId, Data) ->
  rest_client:request(put, j(BaseUri, ["node",PnodeId,"properties"]), Data).

setAllRelationshipProperties(BaseUri, PrelationshipId, Data) ->
  rest_client:request(put, j(BaseUri, ["relationship",PrelationshipId,"properties"]), Data).

setNodeProperty(BaseUri, PnodeId, Pkey, Data) ->
  rest_client:request(put, j(BaseUri, ["node",PnodeId,"properties",Pkey]), Data).

setRelationshipProperty(BaseUri, PrelationshipId, Pkey, Data) ->
  rest_client:request(put, j(BaseUri, ["relationship",PrelationshipId,"properties",Pkey]), Data).

singlePath(BaseUri, PnodeId, Data) ->
  rest_client:request(post, j(BaseUri, ["node",PnodeId,"path"]), Data).

traverse(BaseUri, PnodeId, PreturnType, Data) ->
  rest_client:request(post, j(BaseUri, ["node",PnodeId,"traverse",PreturnType]), Data).

%%%_* Internals --------------------------------------------------------
j(BaseUri, Path0) ->
  Path = filename:join(lists:map(fun s/1, Path0)),
  case lists:last(BaseUri) of
    $/ -> BaseUri ++ Path;
    _  -> BaseUri ++ "/" ++ Path
  end.

error({error, _}=Error) -> Error;
error(Error)            -> error({error, Error}).

%%%_* Helpers ----------------------------------------------------------
kf(Key, List) -> kf(Key, List, undefined).

kf(Key, List, Default) ->
  case lists:keyfind(Key, 1, List) of
    {Key, Value} -> Value;
    false        -> Default
  end.

b(B) when is_binary(B) -> unicode:characters_to_list(B);
b(S) when is_list(S)   -> unicode:characters_to_binary(S).

s(A) when is_atom(A)    -> erlang:atom_to_list(A);
s(I) when is_integer(I) -> erlang:integer_to_list(I);
s(F) when is_float(F)   -> erlang:float_to_list(F);
s(S)                    -> S.

%%% Mode: Erlang
%%% End.
