PROJECT = neo4j

DEPS = hackney jsonx
dep_hackney = https://github.com/benoitc/hackney
dep_jsonx = https://github.com/iskra/jsonx

PLT_APPS ?= asn1 compiler crypto erts inets kernel public_key stdlib ssl syntax_tools

include erlang.mk
