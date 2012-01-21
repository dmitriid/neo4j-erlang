%%% @doc Provides client for Neo4j's REST API v1.5
-module(neo4j_api, [URI]).

%%%_* Exports ==========================================================
%%%_* High Level API ---------------------------------------------------
-export([ getBaseUri/0
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
  URI.

performCypherQuery(Query) ->
  invokeGraphDatabaseExtension("CypherPlugin", "execute_query", Query).

performGremlinScript(Script) ->
  invokeGraphDatabaseExtension("GremlinPlugin", "execute_script", Script).

version() ->
  "1.5".

%%%_* Low Level API ----------------------------------------------------
addToNodeIndex(PindexName, Data) ->
  rest_client:request(post, URI++"index/node/"++PindexName, Data).

addToRelationshipIndex(PindexName, Data) ->
  rest_client:request(post, URI++"index/relationship/"++PindexName, Data).

allPaths(PnodeId, Data) ->
  rest_client:request(post, URI++"node/"++PnodeId++"/paths", Data).

createNode(Data) ->
  rest_client:request(post, URI++"node", Data).

createPagedTraverser(PnodeId, PreturnType, Data) ->
  rest_client:request(post, URI++"node/"++PnodeId++"/paged/traverse/"++PreturnType, Data).

createRelationship(PnodeId, Data) ->
  rest_client:request(post, URI++"node/"++PnodeId++"/relationships", Data).

deleteAllNodeProperties(PnodeId) ->
  rest_client:request(delete, URI++"node/"++PnodeId++"/properties").

deleteAllRelationshipProperties(PrelationshipId) ->
  rest_client:request(delete, URI++"relationship/"++PrelationshipId++"/properties").

deleteFromNodeIndex(Pid, Pvalue, PindexName, Pkey) ->
  rest_client:request(delete, URI++"index/node/"++PindexName++"/"++Pkey++"/"++Pvalue++"/"++Pid).

deleteFromNodeIndexNoKeyValue(Pid, PindexName) ->
  rest_client:request(delete, URI++"index/node/"++PindexName++"/"++Pid).

deleteFromNodeIndexNoValue(Pid, PindexName, Pkey) ->
  rest_client:request(delete, URI++"index/node/"++PindexName++"/"++Pkey++"/"++Pid).

deleteFromRelationshipIndex(Pid, PindexName) ->
  rest_client:request(delete, URI++"index/relationship/"++PindexName++"/"++Pid).

deleteFromRelationshipIndex(Pid, Pvalue, PindexName, Pkey) ->
  rest_client:request(delete, URI++"index/relationship/"++PindexName++"/"++Pkey++"/"++Pvalue++"/"++Pid).

deleteFromRelationshipIndexNoValue(Pid, PindexName, Pkey) ->
  rest_client:request(delete, URI++"index/relationship/"++PindexName++"/"++Pkey++"/"++Pid).

deleteNode(PnodeId) ->
  rest_client:request(delete, URI++"node/"++PnodeId).

deleteNodeIndex(PindexName) ->
  rest_client:request(delete, URI++"index/node/"++PindexName).

deleteNodeProperty(PnodeId, Pkey) ->
  rest_client:request(delete, URI++"node/"++PnodeId++"/properties/"++Pkey).

deleteRelationship(PrelationshipId) ->
  rest_client:request(delete, URI++"relationship/"++PrelationshipId).

deleteRelationshipIndex(PindexName) ->
  rest_client:request(delete, URI++"index/relationship/"++PindexName).

deleteRelationshipProperty(PrelationshipId, Pkey) ->
  rest_client:request(delete, URI++"relationship/"++PrelationshipId++"/properties/"++Pkey).

getAllNodeProperties(PnodeId) ->
  rest_client:request(get, URI++"node/"++PnodeId++"/properties").

getAllRelationshipProperties(PrelationshipId) ->
  rest_client:request(get, URI++"relationship/"++PrelationshipId++"/properties").

getAutoIndexedNodesByQuery(Data) ->
  rest_client:request(get, URI++"index/auto/node", Data).

getAutoIndexedRelationshipsByQuery(Data) ->
  rest_client:request(get, URI++"index/auto/relationship", Data).

getExtensionList(Pname) ->
  rest_client:request(get, URI++"ext/"++Pname).

getExtensionsList() ->
  rest_client:request(get, URI++"ext").

getGraphDatabaseExtensionDescription(Pname, Pmethod) ->
  rest_client:request(get, URI++"ext/"++Pname++"/graphdb/"++Pmethod).

getHtmlBrowseJavascript() ->
  rest_client:request(get, URI++"resource/htmlbrowse.js").

getIndexedNodes(Pvalue, PindexName, Pkey) ->
  rest_client:request(get, URI++"index/node/"++PindexName++"/"++Pkey++"/"++Pvalue).

getIndexedNodes(Pvalue, Pkey) ->
  rest_client:request(get, URI++"index/auto/node/"++Pkey++"/"++Pvalue).

getIndexedNodesByQuery(PindexName, Data) ->
  rest_client:request(get, URI++"index/node/"++PindexName, Data).

getIndexedNodesByQuery(PindexName, Pkey, Data) ->
  rest_client:request(get, URI++"index/node/"++PindexName++"/"++Pkey, Data).

getIndexedRelationships(Pvalue, PindexName, Pkey) ->
  rest_client:request(get, URI++"index/relationship/"++PindexName++"/"++Pkey++"/"++Pvalue).

getIndexedRelationships(Pvalue, Pkey) ->
  rest_client:request(get, URI++"index/auto/relationship/"++Pkey++"/"++Pvalue).

getIndexedRelationshipsByQuery(PindexName, Data) ->
  rest_client:request(get, URI++"index/relationship/"++PindexName, Data).

getIndexedRelationshipsByQuery(PindexName, Pkey, Data) ->
  rest_client:request(get, URI++"index/relationship/"++PindexName++"/"++Pkey, Data).

getNode(PnodeId) ->
  rest_client:request(get, URI++"node/"++PnodeId).

getNodeExtensionDescription(PnodeId, Pname, Pmethod) ->
  rest_client:request(get, URI++"ext/"++Pname++"/node/"++PnodeId++"/"++Pmethod).

getNodeFromIndexUri(Pid, Pvalue, PindexName, Pkey) ->
  rest_client:request(get, URI++"index/node/"++PindexName++"/"++Pkey++"/"++Pvalue++"/"++Pid).

getNodeIndexRoot() ->
  rest_client:request(get, URI++"index/node").

getNodeProperty(PnodeId, Pkey) ->
  rest_client:request(get, URI++"node/"++PnodeId++"/properties/"++Pkey).

getNodeRelationships(PnodeId, Pdirection) ->
  rest_client:request(get, URI++"node/"++PnodeId++"/relationships/"++Pdirection).

getNodeRelationships(PnodeId, Pdirection, Ptypes) ->
  rest_client:request(get, URI++"node/"++PnodeId++"/relationships/"++Pdirection++"/"++Ptypes).

getRelationship(PrelationshipId) ->
  rest_client:request(get, URI++"relationship/"++PrelationshipId).

getRelationshipExtensionDescription(PrelationshipId, Pname, Pmethod) ->
  rest_client:request(get, URI++"ext/"++Pname++"/relationship/"++PrelationshipId++"/"++Pmethod).

getRelationshipFromIndexUri(Pid, Pvalue, PindexName, Pkey) ->
  rest_client:request(get, URI++"index/relationship/"++PindexName++"/"++Pkey++"/"++Pvalue++"/"++Pid).

getRelationshipIndexRoot() ->
  rest_client:request(get, URI++"index/relationship").

getRelationshipProperty(PrelationshipId, Pkey) ->
  rest_client:request(get, URI++"relationship/"++PrelationshipId++"/properties/"++Pkey).

getRelationshipTypes() ->
  rest_client:request(get, URI++"relationship/types").

getRoot() ->
  rest_client:request(get, URI).

invokeGraphDatabaseExtension(Pname, Pmethod, Data) ->
  rest_client:request(post, URI++"ext/"++Pname++"/graphdb/"++Pmethod, Data).

invokeNodeExtension(PnodeId, Pname, Pmethod, Data) ->
  rest_client:request(post, URI++"ext/"++Pname++"/node/"++PnodeId++"/"++Pmethod, Data).

invokeRelationshipExtension(PrelationshipId, Pname, Pmethod, Data) ->
  rest_client:request(post, URI++"ext/"++Pname++"/relationship/"++PrelationshipId++"/"++Pmethod, Data).

jsonCreateNodeIndex(Data) ->
  rest_client:request(post, URI++"index/node", Data).

jsonCreateRelationshipIndex(Data) ->
  rest_client:request(post, URI++"index/relationship", Data).

pagedTraverse(PnodeId, PtraverserId, PreturnType) ->
  rest_client:request(get, URI++"node/"++PnodeId++"/paged/traverse/"++PreturnType++"/"++PtraverserId).

performBatchOperations(Data) ->
  rest_client:request(post, URI++"batch", Data).

removePagedTraverser(PnodeId, PtraverserId, PreturnType) ->
  rest_client:request(delete, URI++"node/"++PnodeId++"/paged/traverse/"++PreturnType++"/"++PtraverserId).

setAllNodeProperties(PnodeId, Data) ->
  rest_client:request(put, URI++"node/"++PnodeId++"/properties", Data).

setAllRelationshipProperties(PrelationshipId, Data) ->
  rest_client:request(put, URI++"relationship/"++PrelationshipId++"/properties", Data).

setNodeProperty(PnodeId, Pkey, Data) ->
  rest_client:request(put, URI++"node/"++PnodeId++"/properties/"++Pkey, Data).

setRelationshipProperty(PrelationshipId, Pkey, Data) ->
  rest_client:request(put, URI++"relationship/"++PrelationshipId++"/properties/"++Pkey, Data).

singlePath(PnodeId, Data) ->
  rest_client:request(post, URI++"node/"++PnodeId++"/path", Data).

traverse(PnodeId, PreturnType, Data) ->
  rest_client:request(post, URI++"node/"++PnodeId++"/traverse/"++PreturnType, Data).
