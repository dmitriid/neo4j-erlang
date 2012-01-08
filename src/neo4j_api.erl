%%% @doc Provides client for Neo4j's REST API v1.5
-module(neo4j_api, [URI]).

%%%_* Exports ==========================================================
-export([ addToNodeIndex/1
        , addToRelationshipIndex/1
        , allPaths/1
        , createNode/0
        , createPagedTraverser/3
        , createRelationship/1
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
        , invokeGraphDatabaseExtension/2
        , invokeNodeExtension/3
        , invokeRelationshipExtension/3
        , jsonCreateNodeIndex/0
        , jsonCreateRelationshipIndex/0
        , pagedTraverse/3
        , performBatchOperations/0
        , removePagedTraverser/3
        , setAllNodeProperties/1
        , setAllRelationshipProperties/1
        , setNodeProperty/2
        , setRelationshipProperty/2
        , singlePath/1
        , traverse/2
        , version/0
        ]).

%%%_* Code =============================================================
%%%_* API --------------------------------------------------------------
addToNodeIndex(PindexName) -> rest_client:request(post, URI++"index/node/"++PindexName++"").
addToRelationshipIndex(PindexName) -> rest_client:request(post, URI++"index/relationship/"++PindexName++"").
allPaths(PnodeId) -> rest_client:request(post, URI++"node/"++PnodeId++"/paths").
createNode() -> rest_client:request(post, URI++"node").
createPagedTraverser(PnodeId, PreturnType, Data) -> rest_client:request(post, URI++"node/"++PnodeId++"/paged/traverse/"++PreturnType++"", Data).
createRelationship(PnodeId) -> rest_client:request(post, URI++"node/"++PnodeId++"/relationships").
deleteAllNodeProperties(PnodeId) -> rest_client:request(delete, URI++"node/"++PnodeId++"/properties").
deleteAllRelationshipProperties(PrelationshipId) -> rest_client:request(delete, URI++"relationship/"++PrelationshipId++"/properties").
deleteFromNodeIndex(Pid, Pvalue, PindexName, Pkey) -> rest_client:request(delete, URI++"index/node/"++PindexName++"/"++Pkey++"/"++Pvalue++"/"++Pid++"").
deleteFromNodeIndexNoKeyValue(Pid, PindexName) -> rest_client:request(delete, URI++"index/node/"++PindexName++"/"++Pid++"").
deleteFromNodeIndexNoValue(Pid, PindexName, Pkey) -> rest_client:request(delete, URI++"index/node/"++PindexName++"/"++Pkey++"/"++Pid++"").
deleteFromRelationshipIndex(Pid, PindexName) -> rest_client:request(delete, URI++"index/relationship/"++PindexName++"/"++Pid++"").
deleteFromRelationshipIndex(Pid, Pvalue, PindexName, Pkey) -> rest_client:request(delete, URI++"index/relationship/"++PindexName++"/"++Pkey++"/"++Pvalue++"/"++Pid++"").
deleteFromRelationshipIndexNoValue(Pid, PindexName, Pkey) -> rest_client:request(delete, URI++"index/relationship/"++PindexName++"/"++Pkey++"/"++Pid++"").
deleteNode(PnodeId) -> rest_client:request(delete, URI++"node/"++PnodeId++"").
deleteNodeIndex(PindexName) -> rest_client:request(delete, URI++"index/node/"++PindexName++"").
deleteNodeProperty(PnodeId, Pkey) -> rest_client:request(delete, URI++"node/"++PnodeId++"/properties/"++Pkey++"").
deleteRelationship(PrelationshipId) -> rest_client:request(delete, URI++"relationship/"++PrelationshipId++"").
deleteRelationshipIndex(PindexName) -> rest_client:request(delete, URI++"index/relationship/"++PindexName++"").
deleteRelationshipProperty(PrelationshipId, Pkey) -> rest_client:request(delete, URI++"relationship/"++PrelationshipId++"/properties/"++Pkey++"").
getAllNodeProperties(PnodeId) -> rest_client:request(get, URI++"node/"++PnodeId++"/properties").
getAllRelationshipProperties(PrelationshipId) -> rest_client:request(get, URI++"relationship/"++PrelationshipId++"/properties").
getAutoIndexedNodesByQuery(Data) -> rest_client:request(get, URI++"index/auto/node", Data).
getAutoIndexedRelationshipsByQuery(Data) -> rest_client:request(get, URI++"index/auto/relationship", Data).
getExtensionList(Pname) -> rest_client:request(get, URI++"ext/"++Pname++"").
getExtensionsList() -> rest_client:request(get, URI++"ext").
getGraphDatabaseExtensionDescription(Pname, Pmethod) -> rest_client:request(get, URI++"ext/"++Pname++"/graphdb/"++Pmethod++"").
getHtmlBrowseJavascript() -> rest_client:request(get, URI++"resource/htmlbrowse.js").
getIndexedNodes(Pvalue, PindexName, Pkey) -> rest_client:request(get, URI++"index/node/"++PindexName++"/"++Pkey++"/"++Pvalue++"").
getIndexedNodes(Pvalue, Pkey) -> rest_client:request(get, URI++"index/auto/node/"++Pkey++"/"++Pvalue++"").
getIndexedNodesByQuery(PindexName, Data) -> rest_client:request(get, URI++"index/node/"++PindexName++"", Data).
getIndexedNodesByQuery(PindexName, Pkey, Data) -> rest_client:request(get, URI++"index/node/"++PindexName++"/"++Pkey++"", Data).
getIndexedRelationships(Pvalue, PindexName, Pkey) -> rest_client:request(get, URI++"index/relationship/"++PindexName++"/"++Pkey++"/"++Pvalue++"").
getIndexedRelationships(Pvalue, Pkey) -> rest_client:request(get, URI++"index/auto/relationship/"++Pkey++"/"++Pvalue++"").
getIndexedRelationshipsByQuery(PindexName, Data) -> rest_client:request(get, URI++"index/relationship/"++PindexName++"", Data).
getIndexedRelationshipsByQuery(PindexName, Pkey, Data) -> rest_client:request(get, URI++"index/relationship/"++PindexName++"/"++Pkey++"", Data).
getNode(PnodeId) -> rest_client:request(get, URI++"node/"++PnodeId++"").
getNodeExtensionDescription(PnodeId, Pname, Pmethod) -> rest_client:request(get, URI++"ext/"++Pname++"/node/"++PnodeId++"/"++Pmethod++"").
getNodeFromIndexUri(Pid, Pvalue, PindexName, Pkey) -> rest_client:request(get, URI++"index/node/"++PindexName++"/"++Pkey++"/"++Pvalue++"/"++Pid++"").
getNodeIndexRoot() -> rest_client:request(get, URI++"index/node").
getNodeProperty(PnodeId, Pkey) -> rest_client:request(get, URI++"node/"++PnodeId++"/properties/"++Pkey++"").
getNodeRelationships(PnodeId, Pdirection) -> rest_client:request(get, URI++"node/"++PnodeId++"/relationships/"++Pdirection++"").
getNodeRelationships(PnodeId, Pdirection, Ptypes) -> rest_client:request(get, URI++"node/"++PnodeId++"/relationships/"++Pdirection++"/"++Ptypes++"").
getRelationship(PrelationshipId) -> rest_client:request(get, URI++"relationship/"++PrelationshipId++"").
getRelationshipExtensionDescription(PrelationshipId, Pname, Pmethod) -> rest_client:request(get, URI++"ext/"++Pname++"/relationship/"++PrelationshipId++"/"++Pmethod++"").
getRelationshipFromIndexUri(Pid, Pvalue, PindexName, Pkey) -> rest_client:request(get, URI++"index/relationship/"++PindexName++"/"++Pkey++"/"++Pvalue++"/"++Pid++"").
getRelationshipIndexRoot() -> rest_client:request(get, URI++"index/relationship").
getRelationshipProperty(PrelationshipId, Pkey) -> rest_client:request(get, URI++"relationship/"++PrelationshipId++"/properties/"++Pkey++"").
getRelationshipTypes() -> rest_client:request(get, URI++"relationship/types").
getRoot() -> rest_client:request(get, URI++"").
invokeGraphDatabaseExtension(Pname, Pmethod) -> rest_client:request(post, URI++"ext/"++Pname++"/graphdb/"++Pmethod++"").
invokeNodeExtension(PnodeId, Pname, Pmethod) -> rest_client:request(post, URI++"ext/"++Pname++"/node/"++PnodeId++"/"++Pmethod++"").
invokeRelationshipExtension(PrelationshipId, Pname, Pmethod) -> rest_client:request(post, URI++"ext/"++Pname++"/relationship/"++PrelationshipId++"/"++Pmethod++"").
jsonCreateNodeIndex() -> rest_client:request(post, URI++"index/node").
jsonCreateRelationshipIndex() -> rest_client:request(post, URI++"index/relationship").
pagedTraverse(PnodeId, PtraverserId, PreturnType) -> rest_client:request(get, URI++"node/"++PnodeId++"/paged/traverse/"++PreturnType++"/"++PtraverserId++"").
performBatchOperations() -> rest_client:request(post, URI++"batch").
removePagedTraverser(PnodeId, PtraverserId, PreturnType) -> rest_client:request(delete, URI++"node/"++PnodeId++"/paged/traverse/"++PreturnType++"/"++PtraverserId++"").
setAllNodeProperties(PnodeId) -> rest_client:request(put, URI++"node/"++PnodeId++"/properties").
setAllRelationshipProperties(PrelationshipId) -> rest_client:request(put, URI++"relationship/"++PrelationshipId++"/properties").
setNodeProperty(PnodeId, Pkey) -> rest_client:request(put, URI++"node/"++PnodeId++"/properties/"++Pkey++"").
setRelationshipProperty(PrelationshipId, Pkey) -> rest_client:request(put, URI++"relationship/"++PrelationshipId++"/properties/"++Pkey++"").
singlePath(PnodeId) -> rest_client:request(post, URI++"node/"++PnodeId++"/path").
traverse(PnodeId, PreturnType) -> rest_client:request(post, URI++"node/"++PnodeId++"/traverse/"++PreturnType++"").
version() -> "1.5".
