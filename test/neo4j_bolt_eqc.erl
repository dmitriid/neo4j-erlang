%%%-------------------------------------------------------------------
%%% @doc Quickcheck tests for neo4j's bolt protocol
%%%
%%%      Documentation:
%%%        - https://github.com/nigelsmall/bolt-howto/blob/master/packstream.py
%%%        - https://github.com/nigelsmall/bolt-howto
%%%
%%% @author Dmitrii Dimandt
%%% @copyright (C) 2015 Dmitrii Dimandt
%%%-------------------------------------------------------------------

-module(neo4j_bolt_eqc).
-compile(export_all).

%%_* Includes ==================================================================
-include_lib("eqc/include/eqc.hrl").

%%_* Properties ================================================================
prop_deserialize_serialize() ->
  ?FORALL( Value
         , gen_data()
         , begin
             Value == neo4j_bolt:deserialize(neo4j_bolt:serialize(Value))
           end
         ).


%%_* Generators ================================================================

gen_data() ->
  frequency([ {1,  null}
            , {1,  bool()}
            , {10, int()}
            , {5,  largeint()}
            , {10, gen_binary(0)}
            , {10, gen_binary(15)}
            , {1,  gen_binary(255)}
            , {1,  gen_binary(65535)}
            %% For the adventurous. This will create 4 GB binaries!
            %, {1, gen_binary(4294967295)}
            ]).


gen_binary(MaxSize) ->
  ?SIZED(Sz, case Sz >= MaxSize of
               true -> binary(MaxSize);
               false -> binary(MaxSize + Sz)
             end).