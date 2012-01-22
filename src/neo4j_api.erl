%%% @doc Provides client for Neo4j's REST API v1.5
-module(neo4j_api, [BaseURI]).

-compile({no_auto_import, [ error/1 ]}).

%%%_* Exports ==========================================================
%%%_* High Level API ---------------------------------------------------
-export([ getBaseUri/0
        , getNodeId/1
        , performCypherQuery/1
        , performGremlinScript/1
        , version/0
        ]).

%%%_* Low Level API ----------------------------------------------------
-export([ addToNodeIndex/2
        , addToRelationshipIndex/2
        , allPaths/2
        , createNode/1
        , createPagedTraverser/3
        , createRelationship/2
        , deleteAllNodeProperties/1
        , deleteAllRelationshipProperties/1
        , deleteFromNodeIndex/4
        , deleteFromNodeIndexNoKeyValue/2
        , deleteFromNodeIndexNoValue/3
        , deleteFromRelationshipIndex/2
        , deleteFromRelationshipIndex/4
        , deleteFromRelationshipIndexNoValue/3
        , deleteNode/1
        , deleteNodeIndex/1
        , deleteNodeProperty/2
        , deleteRelationship/1
        , deleteRelationshipIndex/1
        , deleteRelationshipProperty/2
        , getAllNodeProperties/1
        , getAllRelationshipProperties/1
        , getAutoIndexedNodesByQuery/1
        , getAutoIndexedRelationshipsByQuery/1
        , getExtensionList/1
        , getExtensionsList/0
        , getGraphDatabaseExtensionDescription/2
        , getHtmlBrowseJavascript/0
        , getIndexedNodes/2
        , getIndexedNodes/3
        , getIndexedNodesByQuery/2
        , getIndexedNodesByQuery/3
        , getIndexedRelationships/2
        , getIndexedRelationships/3
        , getIndexedRelationshipsByQuery/2
        , getIndexedRelationshipsByQuery/3
        , getNode/1
        , getNodeExtensionDescription/3
        , getNodeFromIndexUri/4
        , getNodeIndexRoot/0
        , getNodeProperty/2
        , getNodeRelationships/2
        , getNodeRelationships/3
        , getRelationship/1
        , getRelationshipExtensionDescription/3
        , getRelationshipFromIndexUri/4
        , getRelationshipIndexRoot/0
        , getRelationshipProperty/2
        , getRelationshipTypes/0
        , getRoot/0
        , invokeGraphDatabaseExtension/3
        , invokeNodeExtension/4
        , invokeRelationshipExtension/4
        , jsonCreateNodeIndex/1
        , jsonCreateRelationshipIndex/1
        , pagedTraverse/3
        , performBatchOperations/1
        , removePagedTraverser/3
        , setAllNodeProperties/2
        , setAllRelationshipProperties/2
        , setNodeProperty/3
        , setRelationshipProperty/3
        , singlePath/2
        , traverse/3
        ]).

%%%_* Code =============================================================
%%%_* High Level API ---------------------------------------------------
getBaseUri() ->
  j([]).

getNodeId(Node) ->
  try
    NodeURI = erlang:binary_to_list(kf(b("self"), Node)),
    NodeId = erlang:list_to_integer(filename:basename(NodeURI)),
    {ok, NodeId}
  catch _:_ -> error(undefined)
  end.

performCypherQuery(Query) ->
  invokeGraphDatabaseExtension("CypherPlugin", "execute_query", Query).

performGremlinScript(Script) ->
  invokeGraphDatabaseExtension("GremlinPlugin", "execute_script", Script).

version() ->
  "1.5".

%%%_* Low Level API ----------------------------------------------------
addToNodeIndex(PindexName, Data) ->
  rest_client:request(post, j(["index","node",PindexName]), Data).

addToRelationshipIndex(PindexName, Data) ->
  rest_client:request(post, j(["index","relationship",PindexName]), Data).

allPaths(PnodeId, Data) ->
  rest_client:request(post, j(["node",PnodeId,"paths"]), Data).

createNode(Data) ->
  rest_client:request(post, j(["node"]), Data).

createPagedTraverser(PnodeId, PreturnType, Data) ->
  rest_client:request(post, j(["node",PnodeId,"paged","traverse",PreturnType]), Data).

createRelationship(PnodeId, Data) ->
  rest_client:request(post, j(["node",PnodeId,"relationships"]), Data).

deleteAllNodeProperties(PnodeId) ->
  rest_client:request(delete, j(["node",PnodeId,"properties"])).

deleteAllRelationshipProperties(PrelationshipId) ->
  rest_client:request(delete, j(["relationship",PrelationshipId,"properties"])).

deleteFromNodeIndex(Pvalue, PindexName, Pkey, Pid) ->
  rest_client:request(delete, j(["index","node",PindexName,Pkey,Pvalue,Pid])).

deleteFromNodeIndexNoKeyValue(PindexName, Pid) ->
  rest_client:request(delete, j(["index","node",PindexName,Pid])).

deleteFromNodeIndexNoValue(PindexName, Pkey, Pid) ->
  rest_client:request(delete, j(["index","node",PindexName,Pkey,Pid])).

deleteFromRelationshipIndex(PindexName, Pid) ->
  rest_client:request(delete, j(["index","relationship",PindexName,Pid])).

deleteFromRelationshipIndex(Pvalue, PindexName, Pkey, Pid) ->
  rest_client:request(delete, j(["index","relationship",PindexName,Pkey,Pvalue,Pid])).

deleteFromRelationshipIndexNoValue(PindexName, Pkey, Pid) ->
  rest_client:request(delete, j(["index","relationship",PindexName,Pkey,Pid])).

deleteNode(PnodeId) ->
  rest_client:request(delete, j(["node",PnodeId])).

deleteNodeIndex(PindexName) ->
  rest_client:request(delete, j(["index","node",PindexName])).

deleteNodeProperty(PnodeId, Pkey) ->
  rest_client:request(delete, j(["node",PnodeId,"properties",Pkey])).

deleteRelationship(PrelationshipId) ->
  rest_client:request(delete, j(["relationship",PrelationshipId])).

deleteRelationshipIndex(PindexName) ->
  rest_client:request(delete, j(["index","relationship",PindexName])).

deleteRelationshipProperty(PrelationshipId, Pkey) ->
  rest_client:request(delete, j(["relationship",PrelationshipId,"properties",Pkey])).

getAllNodeProperties(PnodeId) ->
  rest_client:request(get, j(["node",PnodeId,"properties"])).

getAllRelationshipProperties(PrelationshipId) ->
  rest_client:request(get, j(["relationship",PrelationshipId,"properties"])).

getAutoIndexedNodesByQuery(Data) ->
  rest_client:request(get, j(["index","auto","node"]), Data).

getAutoIndexedRelationshipsByQuery(Data) ->
  rest_client:request(get, j(["index","auto","relationship"]), Data).

getExtensionList(Pname) ->
  rest_client:request(get, j(["ext",Pname])).

getExtensionsList() ->
  rest_client:request(get, j(["ext"])).

getGraphDatabaseExtensionDescription(Pname, Pmethod) ->
  rest_client:request(get, j(["ext",Pname,"graphdb",Pmethod])).

getHtmlBrowseJavascript() ->
  rest_client:request(get, j(["resource","htmlbrowse.js"])).

getIndexedNodes(PindexName, Pkey, Pvalue) ->
  rest_client:request(get, j(["index","node",PindexName,Pkey,Pvalue])).

getIndexedNodes(Pkey, Pvalue) ->
  rest_client:request(get, j(["index","auto","node",Pkey,Pvalue])).

getIndexedNodesByQuery(PindexName, Data) ->
  rest_client:request(get, j(["index","node",PindexName]), Data).

getIndexedNodesByQuery(PindexName, Pkey, Data) ->
  rest_client:request(get, j(["index","node",PindexName,Pkey]), Data).

getIndexedRelationships(PindexName, Pkey, Pvalue) ->
  rest_client:request(get, j(["index","relationship",PindexName,Pkey,Pvalue])).

getIndexedRelationships(Pkey, Pvalue) ->
  rest_client:request(get, j(["index","auto","relationship",Pkey,Pvalue])).

getIndexedRelationshipsByQuery(PindexName, Data) ->
  rest_client:request(get, j(["index","relationship",PindexName]), Data).

getIndexedRelationshipsByQuery(PindexName, Pkey, Data) ->
  rest_client:request(get, j(["index","relationship",PindexName,Pkey]), Data).

getNode(PnodeId) ->
  rest_client:request(get, j(["node",PnodeId])).

getNodeExtensionDescription(Pname, PnodeId, Pmethod) ->
  rest_client:request(get, j(["ext",Pname,"node",PnodeId,Pmethod])).

getNodeFromIndexUri(PindexName, Pkey, Pvalue, Pid) ->
  rest_client:request(get, j(["index","node",PindexName,Pkey,Pvalue,Pid])).

getNodeIndexRoot() ->
  rest_client:request(get, j(["index","node"])).

getNodeProperty(PnodeId, Pkey) ->
  rest_client:request(get, j(["node",PnodeId,"properties",Pkey])).

getNodeRelationships(PnodeId, Pdirection)
  when Pdirection =:= in orelse Pdirection =:= "in" orelse
       Pdirection =:= out orelse Pdirection =:= "out" ->
  rest_client:request(get, j(["node",PnodeId,"relationships",Pdirection])).

getNodeRelationships(PnodeId, Pdirection, Ptypes)
  when Pdirection =:= in orelse Pdirection =:= "in" orelse
       Pdirection =:= out orelse Pdirection =:= "out" ->
  rest_client:request(get, j(["node",PnodeId,"relationships",Pdirection,Ptypes])).

getRelationship(PrelationshipId) ->
  rest_client:request(get, j(["relationship",PrelationshipId])).

getRelationshipExtensionDescription(Pname, PrelationshipId, Pmethod) ->
  rest_client:request(get, j(["ext",Pname,"relationship",PrelationshipId,Pmethod])).

getRelationshipFromIndexUri(PindexName, Pkey, Pvalue, Pid) ->
  rest_client:request(get, j(["index","relationship",PindexName,Pkey,Pvalue,Pid])).

getRelationshipIndexRoot() ->
  rest_client:request(get, j(["index","relationship"])).

getRelationshipProperty(PrelationshipId, Pkey) ->
  rest_client:request(get, j(["relationship",PrelationshipId,"properties",Pkey])).

getRelationshipTypes() ->
  rest_client:request(get, j(["relationship","types"])).

getRoot() ->
  rest_client:request(get, j([])).

invokeGraphDatabaseExtension(Pname, Pmethod, Data) ->
  rest_client:request(post, j(["ext",Pname,"graphdb",Pmethod]), Data).

invokeNodeExtension(Pname, PnodeId, Pmethod, Data) ->
  rest_client:request(post, j(["ext",Pname,"node",PnodeId,Pmethod]), Data).

invokeRelationshipExtension(Pname, PrelationshipId, Pmethod, Data) ->
  rest_client:request(post, j(["ext",Pname,"relationship",PrelationshipId,Pmethod]), Data).

jsonCreateNodeIndex(Data) ->
  rest_client:request(post, j(["index","node"]), Data).

jsonCreateRelationshipIndex(Data) ->
  rest_client:request(post, j(["index","relationship"]), Data).

pagedTraverse(PnodeId, PreturnType, PtraverserId) ->
  rest_client:request(get, j(["node",PnodeId,"paged","traverse",PreturnType,PtraverserId])).

performBatchOperations(Data) ->
  rest_client:request(post, j(["batch"]), Data).

removePagedTraverser(PnodeId, PreturnType, PtraverserId) ->
  rest_client:request(delete, j(["node",PnodeId,"paged","traverse",PreturnType,PtraverserId])).

setAllNodeProperties(PnodeId, Data) ->
  rest_client:request(put, j(["node",PnodeId,"properties"]), Data).

setAllRelationshipProperties(PrelationshipId, Data) ->
  rest_client:request(put, j(["relationship",PrelationshipId,"properties"]), Data).

setNodeProperty(PnodeId, Pkey, Data) ->
  rest_client:request(put, j(["node",PnodeId,"properties",Pkey]), Data).

setRelationshipProperty(PrelationshipId, Pkey, Data) ->
  rest_client:request(put, j(["relationship",PrelationshipId,"properties",Pkey]), Data).

singlePath(PnodeId, Data) ->
  rest_client:request(post, j(["node",PnodeId,"path"]), Data).

traverse(PnodeId, PreturnType, Data) ->
  rest_client:request(post, j(["node",PnodeId,"traverse",PreturnType]), Data).

%%%_* Internals --------------------------------------------------------
j([])    -> BaseURI;
j(Path0) ->
  Path = filename:join(lists:map(fun s/1, Path0)),
  case lists:last(BaseURI) of
    $/ -> BaseURI++Path;
    _  -> BaseURI++"/"++Path
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
