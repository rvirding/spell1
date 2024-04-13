-module(spell1_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

lfe_test_() ->
    {foreach, spell1_testing:setup(), spell1_testing:teardown(),
     [
      fun run_group/0
     ]}.

run_group() ->
    ok.
