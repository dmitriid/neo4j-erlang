PROJECT = neo4j

DEPS = hackney jiffy
# this commit has a fix for certifi to build with erlang.mk
dep_hackney = git https://github.com/benoitc/hackney fd706efd73c2c906
dep_jiffy = git https://github.com/davisp/jiffy 0.14.4


PLT_APPS ?= asn1 compiler crypto erts inets kernel public_key stdlib ssl syntax_tools

CT_SUITES ?= neo4j

include erlang.mk
