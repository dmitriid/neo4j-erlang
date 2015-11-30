%%%-------------------------------------------------------------------
%%% @doc Unit tests for neo4j's bolt protocol
%%%
%%%      Documentation:
%%%        - https://github.com/nigelsmall/bolt-howto/blob/master/packstream.py
%%%        - https://github.com/nigelsmall/bolt-howto
%%%
%%% @author Dmitrii Dimandt
%%% @copyright (C) 2015 Dmitrii Dimandt
%%%-------------------------------------------------------------------

-module(neo4j_bolt_tests).

%%_* Includes ==================================================================
-include_lib("eunit/include/eunit.hrl").

serialize_null_test() ->
  ?assertEqual( <<16#C0/big-signed-integer>>
              , neo4j_bolt:serialize(null)).

serialize_boolean_test() ->
  ?assertEqual( <<16#C3/big-signed-integer>>
              , neo4j_bolt:serialize(true)),
  ?assertEqual( <<16#C2/big-signed-integer>>
              , neo4j_bolt:serialize(false)).

serialize_float_test() ->
  ?assertEqual                      (
    <<16#C1/integer, 16#3F/integer, 16#F1/integer
    , 16#99/integer, 16#99/integer, 16#99/integer, 16#99/integer
    , 16#99/integer, 16#9A/integer>>,
    neo4j_bolt:serialize(1.1)       ),
  ?assertEqual                      (
    <<16#C1/integer, 16#BF/integer, 16#F1/integer
    , 16#99/integer, 16#99/integer, 16#99/integer, 16#99/integer
    , 16#99/integer, 16#9A/integer>>,
    neo4j_bolt:serialize(-1.1)      ).

serialize_tiny_int_test() ->
  ?assertEqual( <<1:1/big-signed-integer-unit:8>>
              , neo4j_bolt:serialize(1)).

serialize_int_8_test() ->
  ?assertEqual( <<16#C8:1/big-signed-integer-unit:8
                , -100:1/big-signed-integer-unit:8>>
              , neo4j_bolt:serialize(-100)).

serialize_int_16_test() ->
  ?assertEqual( <<16#C9:1/big-signed-integer-unit:8
                , -1000:2/big-signed-integer-unit:8>>
              , neo4j_bolt:serialize(-1000)),
  ?assertEqual( <<16#C9:1/big-signed-integer-unit:8
                , 1000:2/big-signed-integer-unit:8>>
              , neo4j_bolt:serialize(1000)).

serialize_int_32_test() ->
  ?assertEqual( <<16#CA:1/big-signed-integer-unit:8
                , -1000000:4/big-signed-integer-unit:8>>
              , neo4j_bolt:serialize(-1000000)),
  ?assertEqual( <<16#CA:1/big-signed-integer-unit:8
                , 1000000:4/big-signed-integer-unit:8>>
              , neo4j_bolt:serialize(1000000)).

serialize_int_64_test() ->
  ?assertEqual( <<16#CB:1/big-signed-integer-unit:8
                , -1000000000000:8/big-signed-integer-unit:8>>
              , neo4j_bolt:serialize(-1000000000000)),
  ?assertEqual( <<16#CB:1/big-signed-integer-unit:8
                , 1000000000000:8/big-signed-integer-unit:8>>
              , neo4j_bolt:serialize(1000000000000)).

serialize_empty_text_test() ->
  ?assertEqual( <<16#80:1/big-signed-integer-unit:8>>
              , neo4j_bolt:serialize(<<>>)).

serialize_text_up_to_15_bytes_test() ->
  ?assertEqual( <<16#81:1/big-signed-integer-unit:8
                , 16#61:1/big-signed-integer-unit:8>>
              , neo4j_bolt:serialize(<<"a">>)).

serialize_text_up_to_255_bytes_test() ->
  ?assertEqual( <<16#D0:1/big-signed-integer-unit:8
                , 16#1A:1/big-signed-integer-unit:8
                , "abcdefghijklmnopqrstuvwxyz"
                >>
              , neo4j_bolt:serialize(<<"abcdefghijklmnopqrstuvwxyz">>)).