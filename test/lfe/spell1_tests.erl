-module(spell1_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

all_test_() ->
    {foreach, setup(), teardown(),
     [
      fun lfe_group/0
     ]}.

lfe_group() ->
    spell1_testing:check_files(lspell1, "test/lfe").


setup() ->
    spell1_testing:setup().

teardown() ->
    spell1_testing:teardown().
