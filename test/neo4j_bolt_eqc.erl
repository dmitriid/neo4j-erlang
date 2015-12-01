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
         , eqc:collect(Value, begin
             Value == neo4j_bolt:deserialize(neo4j_bolt:serialize(Value))
           end
         )).


%%_* Generators ================================================================

gen_data() ->
  frequency([{70, gen_simple()}
            ,{30, gen_complex()}]).

gen_simple() ->
  frequency([ {1, null}
            , {1, bool()}
            , {10, int()}
            , {10, real()}
            , {5, largeint()}
            , {10, gen_binary(0)}
            , {10, gen_binary(15)}
            , {1,  gen_binary(255)}
            , {1,  gen_binary(65535)}
            %% For the adventurous. This will create 4 GB binaries!
            %, {1, gen_binary(4294967295)}
            ]).

gen_complex() ->
  frequency([{20, gen_list(0)}
            ,{20, gen_list(15)}]).


gen_binary(MinSize) ->
  frequency([ {10, binary(MinSize)}
            , {5, ?SIZED(Sz, case Sz >= MinSize of
                               true -> binary(MinSize);
                               false -> binary(MinSize + Sz)
                             end)}]).

gen_list(MinSize) ->
  frequency([ {60, vector(MinSize, gen_simple())}
            , {30, ?SIZED(Sz, vector(MinSize + Sz, gen_simple()))}
            , {5, ?SIZED(Sz, vector(MinSize, ?LAZY(vector(Sz div 2, gen_simple()))))}
            , {4, ?SIZED(Sz, vector(MinSize + Sz, ?LAZY(vector(Sz div 2, gen_simple()))))}
            , {1, ?SIZED(Sz, vector(MinSize + Sz, ?LAZY(vector(Sz div 2, gen_complex()))))}
            ]).