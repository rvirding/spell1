%% Copyright (c) 2009-2015 Robert Virding
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% File    : spell1.erl
%% Author  : Robert Virding
%% Purpose : A simple LL(1) parser generator.

-module(spell1_core).

-export([init_grammar/0,format_error/1,
         add_root/2,add_terminals/2,add_non_terminals/2,add_rule/5,
         get_root/1,
         make_grammar/1,
         output_entry/2,output_table/2,output_reduce/2,
         dump_tables/1
        ]).      

%% -compile(export_all).

-define(EPSILON, '$empty').                     %Empty

%% Parser state.
-record(state, {root=[],
                term=[],
                nonterm=[],
                rtab,
                rc,
                ptab,
                table,
                errors=[],
                warnings=[]
               }).


%% Rule
-record(rule, {n,                               %Rule number
               line,                            %Rule line
               name,
               syms=[],
               toks=[],                         %Generated tokens
               first=none                       %FIRST set
              }).

%% Production.
-record(prod, {name,
               first=none,                      %FIRST set
               follow=none                      %FOLLOW set
              }).

%% Table cell.
-record(cell, {nonterm,
               term,
               stack=[],
               n                                %Matching rule number
              }).

%% Errors and warnings.
format_error({ambiguous_prod,Prod}) ->
    io_lib:format("ambiguous production: ~p", [Prod]);
format_error({unknown_symbol,Symb}) ->
    io_lib:format("unknown symbol: ~p", [Symb]);
format_error({left_recursion,Prod}) ->
    io_lib:format("left recursion in ~p", [Prod]);
format_error(undefined_root) -> "undefined root symbol";
format_error(bad_declaration) -> "unknown or bad declaration";
format_error(bad_rule) -> "bad rule".

init_grammar() ->
    #state{root=[],
           term=[?EPSILON],
           nonterm=[],
           rtab=ets:new(rtab, [protected,{keypos,#rule.n}]),
           rc=0,
           ptab=ets:new(ptab, [protected,{keypos,#prod.name}]),
           table=ets:new(table, [protected,bag,{keypos,#prod.name}])}.

%% add_root(RootName, State) -> State.
%% add_terminals(Terminals, State) -> State.
%% add_non_terminals(NonTerminals, State) -> State.
%% add_rule(Line, Name, Syms, Toks, State) -> State.
%%  Add the various sections to the grammar state.

add_root(Root, St) ->
    St#state{root=Root}.

add_terminals(Terms, #state{term=Ts0}=St) ->
    Ts1 = lists:foldl(fun (T, Ts) -> ordsets:add_element(T, Ts) end,
                      Ts0, Terms),
    St#state{term=Ts1}.

add_non_terminals(Nterms, #state{nonterm=Nts0}=St) ->
    Nts1 = lists:foldl(fun (T, Ts) -> ordsets:add_element(T, Ts) end,
                       Nts0, Nterms),
    St#state{nonterm=Nts1}.

add_rule(Line, Name, Syms, Toks, #state{rtab=Rtab,rc=Rc}=St) ->
    R = #rule{n=Rc,line=Line,name=Name,syms=Syms,toks=Toks,first=[]},
    ets:insert(Rtab, R),
    St#state{rc=Rc+1}.

get_root(#state{root=Root}) ->
    Root.

%% make_grammar(State) ->
%%     {ok,State,Warnings} | {error,Errors,Warnings}.

make_grammar(St0) ->
    case do_passes(passes(), St0) of
        {ok,St1} ->
            {ok,St1,St1#state.warnings};
        {error,#state{errors=Es,warnings=Ws}} ->
            {error,Es,Ws}
    end.
    
do_passes([{do,Fun}|Ps], St0) ->
    case Fun(St0) of
        {ok,St1} -> do_passes(Ps, St1);
        {error,St1} -> {error,St1}
    end;
do_passes([], St) -> {ok,St}.                   %Got to the end, everything ok

passes() ->
    [{do,fun pre_check_grammar/1},
     {do,fun make_first_follow/1},
     {do,fun make_table/1},
     {do,fun post_check_grammar/1}].

%% pre_check_grammar(State) -> ok | {error,Errors}.
%%  Check the grammar before any processing to try and find obvious
%%  errors.

pre_check_grammar(#state{rtab=Rtab,root=Root}=St0) ->
    St1 = if Root =:= [] -> add_error(none, undefined_root, St0);
             true -> St0
          end,
    Cfun = fun (R, St) -> check_rule(R, St) end,
    St2 = ets:foldl(Cfun, St1, Rtab),
    case St2#state.errors of
        [] -> {ok,St2};
        _ -> {error,St2}
    end.

check_rule(#rule{line=L,syms=Syms}=Rule, St0) ->
    %% Check the basic rule structure.
    St1 = case is_proper_list(Syms) of
              true -> St0;
              false -> add_error(L, bad_rule, St0)
          end,
    check_rule_symbols(Rule, St1).

check_rule_symbols(#rule{line=L,name=Name,syms=Syms},
                   #state{term=T,nonterm=Nt}=St0) ->
    St1 = case ordsets:is_element(Name, Nt) of
              true -> St0;
              false -> add_error(L, {unknown_symbol,Name}, St0)
          end,
    Sfun = fun (S, St) ->
                   case ordsets:is_element(S, T) or ordsets:is_element(S, Nt) of
                       true -> St;
                       false -> add_error(L, {unknown_symbol,S}, St)
                   end
           end,
    lists:foldl(Sfun, St1, Syms).

is_proper_list([_|T]) -> is_proper_list(T);
is_proper_list([]) -> true;
is_proper_list(_) -> false.

%% make_first_follow(State) -> {ok,State} | {error,Errors,State}.
%%  Make the FIRST and FOLLOW sets of the productions.

make_first_follow(#state{rtab=Rtab,ptab=Ptab}=St) ->
    MS = {#rule{name='$0',_='_'},[],['$0']},
    Ps = ordsets:from_list(ets:select(Rtab, [MS])),
    %% Initialse the production table.
    lists:foreach(fun (P) ->
                          ets:insert(Ptab, #prod{name=P,first=[],follow=[]})
                  end, Ps),
    %% Can get recursion errors here.
    try
        prod_firsts(St),                        %Write productions
        prod_follows(St),
        {ok,St}
    catch
        error:{spell1_error,Error} ->
            {error,add_error(none, Error, St)}
    end.

%% prod_firsts(State) -> ok.
%%  Build the FIRST sets of the productions.

prod_firsts(#state{ptab=Ptab}=St) ->
    Ps = ets:select(Ptab, [{#prod{name='$0',_='_'},[],['$0']}]),
    lists:foreach(fun (P) -> prod_first(P, St) end, Ps).

%% prod_first(Name, State [, Seen]) -> First.
%%  Build the FIRST set of the production Name. Seen is a stack so we
%%  can better show recursion.

prod_first(Name, St) -> prod_first(Name, St, []).

prod_first(Name, #state{term=Term,rtab=Rtab,ptab=Ptab}=St, Seen0) ->
    %% Check for left recursive rule.
    lists:member(Name, Seen0) andalso
        error({spell1_error,{left_recursion, [Name|Seen0]}}),
    case ordsets:is_element(Name, Term) of
        true -> [Name];
        false ->
            [Prod] = ets:lookup(Ptab, Name),
            %% Check if the production already has FIRST, else make it.
            case Prod of
                #prod{first=[]} ->
                    Seen1 = [Name|Seen0],
                    MS = {#rule{n='$0',name=Name,_='_'},[],['$0']},
                    Ns = ets:select(Rtab, [MS]),
                    Ffun = fun (N, First) ->
                                   W = rule_first(N, St, Seen1),
                                   ordsets:union(W, First)
                           end,
                    First = lists:foldl(Ffun, ordsets:new(), Ns),
                    ets:update_element(Ptab, Name, {#prod.first,First}),
                    First;
                #prod{first=First} -> First
            end
    end.

rule_first(N, #state{rtab=Rtab}=St, Seen) ->
    [#rule{syms=Syms}] = ets:lookup(Rtab, N),
    First  = rule_syms_first(Syms, St, Seen),
    ets:update_element(Rtab, N, {#rule.first,First}),
    First.

rule_syms_first(Ss, St, Seen) ->
    rule_syms_first(Ss, St, ordsets:new(), Seen).

rule_syms_first([S|Ss], #state{term=Term}=St, First0, Seen) ->
    case ordsets:is_element(S, Term) of
        true -> ordsets:add_element(S, First0);
        false ->
            W = prod_first(S, St, Seen),
            case ordsets:is_element(?EPSILON, W) of
                true ->
                    First1 = ordsets:union(ordsets:del_element(?EPSILON, W),
                                           First0),
                    rule_syms_first(Ss, St, First1, Seen);
                false ->
                    ordsets:union(W, First0)
            end
    end;
rule_syms_first([], _, First, _) ->
    ordsets:add_element(?EPSILON, First).

%% prod_follows(State) -> ok.
%%  Build the FOLLOW sets of the productions. Keep looping over all
%%  the rules until no further changes.

prod_follows(#state{rtab=Rtab}=St) ->
    prod_follows_loop(ets:tab2list(Rtab), St).

prod_follows_loop(Rules, St) ->
    Fun = fun (R, Ch) -> Ch or rule_follow(R, St) end,
    case lists:foldl(Fun, false, Rules) of
        true -> prod_follows_loop(Rules, St);
        false -> ok
    end.

%% rule_follow(Rule, St) -> Changed.
%%  Changed is just if working out the FOLLOWs here have changed
%%  anything.

rule_follow(#rule{name=Name,syms=Ss}, St) ->
    syms_follow(Name, Ss, St, false).

syms_follow(Name, [S], #state{term=Term}=St, Ch) ->
    case ordsets:is_element(S, Term) of
        true -> Ch;
        false -> add_follow_to_follow(Name, S, St, Ch)
    end;
syms_follow(Name, [S1,S2|Ss], #state{term=Term}=St, Ch0) ->
    case ordsets:is_element(S1, Term) of
        true -> syms_follow(Name, [S2|Ss], St, Ch0);
        false ->
            First = get_prod_first(S2, St),
            Ch1 = case ordsets:is_element(?EPSILON, First) of
                      true -> add_follow_to_follow(Name, S1, St, Ch0);
                      false -> Ch0
                  end,
            Ch2 = add_first_to_follow(S2, S1, St, Ch1),
            %% Ch1 = case ordsets:is_element(?EPSILON, First) of
            %%           true -> add_follow_to_follow(Name, S1, St, Ch0);
            %%           false -> add_first_to_follow(S2, S1, St, Ch0)
            %%       end,
            syms_follow(Name, [S2|Ss], St, Ch2)
    end;
syms_follow(_, _, _, Ch) -> Ch.

get_prod_first(Name, #state{term=Term,ptab=Ptab}) ->
    case ordsets:is_element(Name, Term) of      %Just to be safe
        true -> [];
        false ->
            ets:lookup_element(Ptab, Name, #prod.first)
    end.

get_prod_follow(Name, #state{ptab=Ptab}) ->
    ets:lookup_element(Ptab, Name, #prod.follow).

add_first_to_follow(From, To, #state{term=Term,ptab=Ptab}, Ch) ->
    First0 = case ordsets:is_element(From, Term) of
                 true -> [From];
                 false ->
                     ets:lookup_element(Ptab, From, #prod.first)
             end,
    First1 = ordsets:del_element(?EPSILON, First0),
    Follow0 = ets:lookup_element(Ptab, To, #prod.follow),
    Follow1 = ordsets:union(First1, Follow0),
    ets:update_element(Ptab, To, {#prod.follow,Follow1}),
    Ch or (Follow0 =/= Follow1).

add_follow_to_follow(From, To, #state{ptab=Ptab}, Ch) ->
    FromF = ets:lookup_element(Ptab, From, #prod.follow),
    ToF0 = ets:lookup_element(Ptab, To, #prod.follow),
    case ordsets:union(FromF, ToF0) of
        ToF0 -> Ch;                             %Same FOLLOW, no update
        ToF1 ->                                 %New FOLLOW, update
            ets:update_element(Ptab, To, {#prod.follow,ToF1}),
            true
    end.

    %% ToF1 = ordsets:union(FromF, ToF0),
    %% ets:update_element(Ptab, To, {#prod.follow,ToF1}),
    %% Ch or (ToF0 =/= ToF1).

%% post_check_grammar(State) -> {ok,State} | {error,State}.
%%  Check the grammar after processing.

post_check_grammar(#state{ptab=Ptab}=St) ->
    post_check_grammar(ets:tab2list(Ptab), St).

post_check_grammar([#prod{name=P}|Ps], St0) ->
    St1 = case check_prod_rules(P, St0) of
              true -> St0;
              false -> add_error(none, {ambiguous_prod,P}, St0)
          end,
    post_check_grammar(Ps, St1);
post_check_grammar([], St) ->
    case St#state.errors of
        [] -> {ok,St};
        _ -> {error,St}
    end.

check_prod_rules(Prod, #state{rtab=Rtab}) ->
    MS = {#rule{name=Prod,first='$0',_='_'},[],['$0']},
    Firsts = ets:select(Rtab, [MS]),
    {Dis,_} = lists:foldl(fun (F, {Dis,Un}) ->
                                  {ordsets:is_disjoint(F, Un) and Dis,
                                   ordsets:union(F, Un)}
                      end, {true,hd(Firsts)}, tl(Firsts)),
    Dis.

%% make_table(State) -> {ok,State}.
%%  Make the table.

make_table(#state{rtab=Rtab,term=Ts,nonterm=Nts,table=Tab}=St0) ->
    make_table(Nts, Ts, Rtab, Tab),
    {ok,St0}.

make_table(Nts, Ts, Rtab, Tab) ->
    lists:foreach(fun (Nt) -> make_table_row(Nt, Ts, Rtab, Tab) end, Nts).

make_table_row(Nt, Ts, Rtab, Tab) ->
    lists:foreach(fun (T) -> make_table_cell(Nt, T, Rtab, Tab) end, Ts).

make_table_cell(Nt, T, Rtab, Tab) ->
    Rs = ets:select(Rtab, [{#rule{name=Nt,_='_'},[],['$_']}]),
    case find_rule(Rs, T) of
        #rule{n=N,syms=Ss} ->
            ets:insert(Tab, #cell{nonterm=Nt,term=T,stack=Ss,n=N});
        none -> ok
    end.

find_rule([#rule{first=Fs}=R|Rs], T) ->
    case ordsets:is_element(T, Fs) of
        true -> R;
        false -> find_rule(Rs, T)
    end;
find_rule([], _) -> none.

%% output_entry(EntryFun, State) -> ok.

output_entry(Efun, #state{root=R}) ->
    Efun(R),
    ok.

%% output_table(RowFun, State) -> ok.
%%  Traverse the table calling RowFun to output a row in the table.

output_table(Rfun, #state{table=Tab}=St) ->
    Cells = ets:tab2list(Tab),
    Cfun = fun (C) -> output_table_cell(Rfun, C, St) end,
    lists:foreach(Cfun, Cells).

output_table_cell(Rfun, #cell{nonterm=Nt,term=?EPSILON,n=N}, St) ->
    Body = [{reduce,N}],
    Follow = get_prod_follow(Nt, St),
    Pfun = fun (T) -> Rfun(Nt, T, Body) end,
    lists:foreach(Pfun, Follow);
output_table_cell(Rfun, #cell{nonterm=Nt,term=T,stack=Stack,n=N}, _) ->
    Body = Stack ++ [{reduce,N}],
    Rfun(Nt, T, Body).

%% output_reduce(RowFun, State) -> ok.
%%  Traverse the rule table calling RowFun for each row of the table.

output_reduce(Rfun, #state{rtab=Rtab}=St) ->
    Fun = fun (R) -> output_reduce_row(Rfun, R, St) end,
    lists:foreach(Fun, lists:keysort(#rule.n, ets:tab2list(Rtab))).

output_reduce_row(Rfun, #rule{n=N,syms=[?EPSILON|_],toks=Toks}, _) ->
    %% EPSILON rules are special in that they don't consume any
    %% tokens!
    Rfun(N, 0, Toks);
output_reduce_row(Rfun, #rule{n=N,syms=Syms,toks=Toks}, _) ->
    Rfun(N, length(Syms), Toks).

%% dump_state(State) -> ok.
%%  Dump all the state data.

dump_tables(G) ->
    io:format("~p\n", [G#state.root]),
    io:format("~p\n", [G#state.term]),
    io:format("~p\n", [G#state.nonterm]),
    io:format("~p\n", [ets:tab2list(G#state.rtab)]),
    io:format("~p\n", [ets:tab2list(G#state.ptab)]),
    io:format("~p\n", [ets:tab2list(G#state.table)]).

%% Errors and warnings.

add_error(Line, E, St) -> add_error(Line, ?MODULE, E, St).

add_error(Line, Mod, E, St) ->
    add_error({Line,Mod,E}, St) .

add_error(Error, St) ->
    St#state{errors=St#state.errors ++ [Error]}.

%% add_warning(Line, W, St) -> add_warning(Line, ?MODULE, W, St).

%% add_warning(Line, Mod, W, St) ->
%%     add_warning({Line,Mod,W}, St).

%% add_warning(Warning, St) ->
%%     St#spell1{warnings=St#spell1.warnings ++ [Warning]}.
