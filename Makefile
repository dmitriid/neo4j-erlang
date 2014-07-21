PROJECT = neo4j

DEPS = hackney jiffy
dep_hackney = https://github.com/benoitc/hackney
dep_jiffy = https://github.com/davisp/jiffy

PLT_APPS ?= asn1 compiler crypto erts inets kernel public_key stdlib ssl syntax_tools

CT_SUITES ?= neo4j

include erlang.mk
