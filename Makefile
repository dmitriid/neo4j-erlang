PROJECT = neo4j
PROJECT_DESCRIPTION = Erlang client library for Neo4J's REST API
PROJECT_VERSION = 0.2.2

DEPS = hackney jiffy
# this commit has a fix for certifi to build with erlang.mk
dep_hackney = git https://github.com/benoitc/hackney fd706efd73c2c906
dep_jiffy = git https://github.com/davisp/jiffy 0.14.4


PLT_APPS ?= asn1 compiler crypto erts inets kernel public_key stdlib ssl syntax_tools

CT_SUITES ?= neo4j

TEST_DEPS = eunit_formatters

EUNIT_OPTS = no_tty, {report, {eunit_progress, [colored, profile]}}

.DEFAULT_GOAL := app

eqc-ci: app
	erlc -o ebin test/*_eqc.erl

include erlang.mk
