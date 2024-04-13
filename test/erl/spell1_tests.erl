-module(spell1_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% TESTS
%%%=============================================================================

erlang_test_() ->
    {foreach, spell1_testing:setup(), spell1_testing:teardown(),
     [
      ?_assert(lfe:form([{string,1,"lfe"}]) == {ok,1,"lfe",[]}),
      ?_assert(lfe:form([{number,1,42}]) == {ok,1,42,[]})
     ]}.
    
