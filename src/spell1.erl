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

-module(spell1).

-export([file/1,file/2,format_error/1]).

%% -compile(export_all).

%% The spell1 state.
-record(spell1, {base="",
                 gdir=".",                      %Grammar directory
                 gfile="",                      %Grammar file
                 odir=".",                      %Output directory
                 efile="",                      %Erlang file
                 ifile="",                      %Include file
                 erlang_code=none,              %Erlang code starts here
                 module=[],                     %The module name
                 rules=[],
                 gram=none,
                 opts=[],
                 errors=[],
                 warnings=[]
                }).

-record(symbol, {line=none,name}).              %Yecc parser symbols

-define(DEFAULT_OPTS, [report,verbose]).
-define(INCLUDE_FILE, "spell1inc.hrl").

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 23).
-define(CATCH_CLAUSE(Class,Reason,Stack),
        Class:Reason:Stack ->).
-else.
-define(CATCH_CLAUSE(Class,Reason,Stack),
        Class:Reason ->
           Stack = erlang:get_stacktrace(),).
-endif.
-else.
-define(CATCH_CLAUSE(Class,Reason,Stack),
        Class:Reason ->
           Stack = erlang:get_stacktrace(),).
-endif.

%% Errors and warnings.
format_error(bad_declaration) -> "unknown or bad declaration".

%% file(FileName [, Options]) -> Result.
%%  Generate a parser from the grammar defined in FileName.

file(File) -> file(File, ?DEFAULT_OPTS).

file(File, Opts) ->
    Ifun = fun () ->
                   Ret = try
                             internal(File, Opts)
                         catch
                             %% Note: macro adds ->
                             ?CATCH_CLAUSE(error, Reason, St)
                                 {error,{Reason,St}}
                         end,
                   exit(Ret)
           end,
    {Pid,Ref} = spawn_monitor(Ifun),
    receive
        {'DOWN',Ref,_,Pid,Res} -> Res
    end.

internal(File, Opts) ->
    St0 = #spell1{gram=spell1_core:init_grammar(Opts),
                  opts=Opts},
    St1 = filenames(File, St0),
    case do_passes(passes(), St1) of
        {ok,St2} -> do_ok_return(St2);
        {error,St2} -> do_error_return(St2)
    end.

%% filenames(File, State) -> State.
%%  The default output dir is the current directory unless an
%%  explicit one has been given in the options.

filenames(File, St) ->
    %% Test for explicit outdir.
    Odir = outdir(St#spell1.opts),
    Dir = filename:dirname(File),
    Base = filename:basename(File, ".spell1"),
    Gfile = filename:join(Dir, Base ++ ".spell1"),
    Efile = Base ++ ".erl",
    Ifile = includefile(St#spell1.opts),
    Module = list_to_atom(Base),
    St#spell1{base=Base,
              gdir=Dir,
              gfile=Gfile,
              odir=Odir,
              efile=filename:join(Odir, Efile),
              ifile=Ifile,
              module=Module}.

outdir(Opts) -> find_opt(outdir, Opts, ".").
includefile(Opts) -> find_opt(includefile, Opts, ?INCLUDE_FILE).

find_opt(Opt, [{Opt,Val}|_],_) -> Val;
find_opt(Opt, [[Opt,Val]|_],_) -> Val;
find_opt(Opt, [_|Opts], Def) -> find_opt(Opt, Opts, Def);
find_opt(_, [], Def) -> Def.
    
do_passes([{do,Fun}|Ps], St0) ->
    case Fun(St0) of
        {ok,St1} -> do_passes(Ps, St1);
        {error,St1} -> {error,St1}
    end;
do_passes([], St) -> {ok,St}.                   %Got to the end, everything ok

passes() ->
    [{do,fun parse_grammar/1},
     {do,fun extract_grammar/1},
     {do,fun make_grammar/1},
     {do,fun output_file/1},
     {do,fun noop/1}].

do_ok_return(#spell1{gfile=Gfile,gram=G,opts=Opts,warnings=Ws}) ->
    when_opt(debug, Opts, fun () -> spell1_core:dump_tables(G) end),
    when_opt(report, Opts, fun () -> list_warnings(Gfile, Ws) end),
    case lists:member(return, Opts) of
        true -> {ok,return_errors(Gfile, Ws)};
        false -> ok
    end.

do_error_return(#spell1{gfile=Gfile,opts=Opts,errors=Es,warnings=Ws}) ->
    when_opt(report, Opts, fun () -> list_errors(Gfile, Es) end),
    when_opt(report, Opts, fun () -> list_warnings(Gfile, Ws) end),
    %% Fix the right return.
    case lists:member(return, Opts) of
        true -> {error,return_errors(Gfile, Es),return_errors(Gfile, Ws)};
        false -> error
    end.

return_errors(_, []) -> [];
return_errors(Gfile, Es) -> [{Gfile,Es}].

list_warnings(F, Ws) ->
    Wfun = fun ({Line,Mod,Warn}) ->
                   Cs = Mod:format_error(Warn),
                   io:format("~s:~w: Warning: ~s\n", [F,Line,Cs])
           end,
    lists:foreach(Wfun, Ws).

list_errors(F, Es) ->
    Efun = fun ({Line,Mod,Error}) ->
                    Cs = Mod:format_error(Error),
                    io:format("~s:~w: ~s\n", [F,Line,Cs])
            end,
    lists:foreach(Efun, Es).

%% when_opt(Option, Options, Fun) -> ok.
%% unless_opt(Option, Options, Fun) -> ok.
%%  Vall Fun when Option is/is not a member of Options.

when_opt(Opt, Opts, Fun) ->
    case lists:member(Opt, Opts) of
        true -> Fun();
        false -> ok
    end.

%% unless_opt(Opt, Opts, Fun) ->
%%     case lists:member(Opt, Opts) of
%%         true -> ok;
%%         false ->  Fun()
%%     end.

%% add_error(Line, E, St) -> add_error(Line, ?MODULE, E, St).

add_error(Line, Mod, E, St) ->
    add_error({Line,Mod,E}, St).

add_error(Error, St) ->
    St#spell1{errors=St#spell1.errors ++ [Error]}.

add_warning(Line, W, St) -> add_warning(Line, ?MODULE, W, St).

add_warning(Line, Mod, W, St) ->
    add_warning({Line,Mod,W}, St).

add_warning(Warning, St) ->
    St#spell1{warnings=St#spell1.warnings ++ [Warning]}.

%% The passes.

noop(St) -> {ok,St}.                            %Dummy pass

%% parse_grammar(State) -> {ok,State} | {error,State}.
%%  Read in the grammar file. We keep reading till the end or until we
%%  read a line "Erlang code." when we store the line number and then
%%  we close the file.

parse_grammar(St) ->
    case file:open(St#spell1.gfile, [read]) of
        {ok,F} ->
            Ret = parse_grammar(F, 1, St),
            file:close(F),
            Ret;
        {error,E} ->
            {error,add_error(none, file, E, St)}
    end.

parse_grammar(F, Line, St0) ->
    case read_grammar(F, Line, St0) of
        {ok,G,NextLine} ->
            case G of
                {#symbol{line=Eline,name='Erlang'},[#symbol{name=code}]} ->
                    {ok,St0#spell1{erlang_code=Eline}};
                _ ->
                    St1 = St0#spell1{rules=St0#spell1.rules ++ [G]},
                    parse_grammar(F, NextLine, St1)
            end;
        {error,Error,_} ->
            {error,add_error(Error, St0)};
        {eof,_} ->
            {ok,St0}
    end.

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 24).
-define(SCAN_RESULT(Ts, Next), {ok, _, Ts, Next}).
-else.
-define(SCAN_RESULT(Ts, Next), {ok, Ts, Next}).
-endif.
-else.
-define(SCAN_RESULT(Ts, Next), {ok, Ts, Next}).
-endif.

read_grammar(F, Line, _St) ->
    case yeccscan:scan(F, '', Line) of
        ?SCAN_RESULT(Ts,Next) ->
            case yeccparser:parse(Ts) of
                {ok,{rule,Rule,{erlang_code,Ets}}} ->
                        {ok,{rule,Rule,Ets},Next};
                {ok,R} -> {ok,R,Next};
                {error,E} ->
                    {error,{Line,yeccparser,E},Next}
            end;
        {eof,Next} ->
            {eof,Next};
        {error,E,Next} ->
            {error,E,Next}
    end.

%% extract_grammar(State) -> {ok,State} | {error,State}.
%%  Extract the relevant forms from the grammar file. For unknown
%%  declarations we just issue a warning and ignore them.

extract_grammar(#spell1{rules=Rs,gram=G0}=St0) ->
    Efun = fun ({rule,[Name|Ss0],Ts}, {G,St}) ->
                   Ss1 = values(Ss0),
                   {spell1_core:add_rule(line(Name), value(Name), Ss1, Ts, G),
                    St};
               ({#symbol{name='Terminals'},Ts}, {G,St}) ->
                   {spell1_core:add_terminals(values(Ts), G),St};
               ({#symbol{name='Nonterminals'},Nts}, {G,St}) ->
                   {spell1_core:add_non_terminals(values(Nts), G),St};
               ({#symbol{name='Rootsymbol'},[Root]}, {G,St}) ->
                   {spell1_core:add_root(value(Root), G),St};
               ({#symbol{line=L},_}, {G,St}) ->
                   %% Ignore unknown declarations.
                   {G,add_warning(L, bad_declaration, St)}
           end,
    {G1,St1} = lists:foldl(Efun, {G0,St0}, Rs),
    {ok,St1#spell1{gram=G1}}.

%% make_grammar(State) -> {ok,State} | {error,State}.
%%  Make the grammar.

make_grammar(#spell1{gram=G0,errors=Es,warnings=Ws}=St) ->
    case spell1_core:make_grammar(G0) of
        {ok,G1,Gws} ->
            {ok,St#spell1{gram=G1,warnings=Ws ++ Gws}};
        {error,Ges,Gws} ->
            {error,St#spell1{errors=Es ++ Ges,warnings=Ws ++ Gws}}
    end.

value(T) -> element(3, T).
values(Ts) -> [ value(T) || T <- Ts ].
line(T) -> element(2, T).

%% output_file(State) -> {ok,State} | {error,State}.

output_file(#spell1{efile=Efile,ifile=Ifile}=St0) ->
    {ok,Inc} = file:open(Ifile, [read]),
    {ok,Out} = file:open(Efile, [write]),
    St1 = output_file(Inc, Out, St0, 1),
    file:close(Inc),
    file:close(Out),
    case St1#spell1.errors of
        [] -> {ok,St1};
        _ -> {error,St1}
    end.

output_file(Inc, Out, St, L) ->
    case io:get_line(Inc, spell1) of
        eof -> St;
        {error,E} -> add_error(E, St);
        Line ->
            case Line of
                "##module" ++ _ -> output_module(Out, St);
                "##export" ++ _ -> output_export(Out, St);
                "##code" ++ _ -> output_user_code(Out, St);
                "##entry" ++ _ -> output_entry(Out, St);
                "##table" ++ _ -> output_table(Out, St);
                "##reduce" ++ _ -> output_reduce(Out, St);
                _ -> io:put_chars(Out, Line)
            end,
            output_file(Inc, Out, St, L+1)
    end.

output_module(Out, #spell1{module=M}) ->
    io:format(Out, "-module(~p).\n", [M]).

output_export(Out, #spell1{gram=G}) ->
    R = spell1_core:get_root(G),
    io:format(Out, "-export([~p/1,~p/2]).\n", [R,R]).

output_user_code(_, #spell1{erlang_code=none}) -> ok;
output_user_code(Out, #spell1{gfile=Gfile,erlang_code=Eline}) ->
    {ok,In} = file:open(Gfile, [read]),
    skip_lines(In, erl_anno:line(Eline)),
    %% output_file_directive(Out, Gfile, Eline),
    output_lines(In, Out),
    file:close(In).

skip_lines(In, L) when L > 0 ->
    case io:get_line(In, spell1) of
        eof -> ok;
        {error,_} -> ok;
        _ -> skip_lines(In, L-1)                %A line
    end;
skip_lines(_, 0) -> ok.

output_lines(In, Out) ->
    case io:get_line(In, spell1) of
        eof -> ok;
        {error,_} -> ok;
        Line ->
            io:put_chars(Out, Line),
            output_lines(In, Out)
    end.

output_entry(Out, #spell1{gram=G}) ->
    R = spell1_core:get_root(G),
    io:format(Out, "~p(Ts) -> parse1([], Ts).\n", [R]),
    io:format(Out, "~p(Cont, Ts) -> parse1(Cont, Ts).\n", [R]).

output_table(Out, #spell1{gram=G}) ->
    %% Define the start state of the grammar.
    R = spell1_core:get_root(G),
    io:format(Out, "start() -> ~p.\n", [R]),
    io:nl(Out),
    %% Now output the table function.
    Rfun = fun (Nt, T, Body) ->
                   io:format(Out, "table(~w, ~w) -> ~w;\n", [Nt,T,Body])
           end,
    spell1_core:output_table(Rfun, G),
    io:put_chars(Out, "table(_, _) -> error.\n").

output_reduce(Out, #spell1{gram=G}) ->
    Rfun = fun (N, SymLen, Toks) -> output_reduce_row(Out, N, SymLen, Toks) end,
    spell1_core:output_reduce(Rfun, G),
    io:put_chars(Out, "reduce(_, _) -> error(function_clause).\n").

%% output_reduce_row(Out, N, Symlen, Tokens) -> ok.
%%  Output one row/clause of the reduce function.

output_reduce_row(Out, N, 0, Toks) ->
    %% EPSILON rules are special in that they don't consume any
    %% tokens!
    L0 = line(hd(Toks)),
    Stoks = subst_pseudo_vars(Toks),            %Substitute in variables
    Stpat = "__Vs",                             %Pass evrything on
    io:format(Out, "reduce(~w, ~s) -> [ ~s | __Vs];\n",
              [N,Stpat,pp_tokens(Stoks, L0)]);
output_reduce_row(Out, N, SymLen, Toks) ->
    L0 = line(hd(Toks)),
    Stoks = subst_pseudo_vars(Toks),            %Substitute in variables
    Stpat =                                     %Stack pattern, always at 
        "[" ++                                  % least one variable
        [ lists:concat(["__",I,","]) || I <- lists:seq(SymLen, 2, -1) ] ++
        "__1|__Vs]",
    io:format(Out, "reduce(~w, ~s) -> [ ~s | __Vs];\n",
              [N,Stpat,pp_tokens(Stoks, L0)]).

subst_pseudo_vars([{atom,L,Atom}=T0|Ts]) ->
    T1 = case atom_to_list(Atom) of
             [$$|Rest] ->
                 try list_to_integer(Rest) of
                     N when N > 0 ->
                         {var,L,list_to_atom("__" ++ Rest)};
                     _ -> T0
                 catch
                     error:_ -> T0
                 end;
             _ -> T0
         end,
    [T1|subst_pseudo_vars(Ts)];
subst_pseudo_vars([T|Ts]) -> [T|subst_pseudo_vars(Ts)];
subst_pseudo_vars([]) -> [].

%% pp_tokens(Tokens, Line) -> [char()].
%%  Prints the tokens keeping the line breaks of the original code. We
%%  wrap it in a begin ... end so it becomes one expression.

pp_tokens(Tokens, Line) ->
    ["begin"," ",pp_tokens(Tokens, Line, none)," ","end"].
    
pp_tokens([T | Ts], Line0, Prev) ->
    Line = erl_scan:line(T),
    [pp_sep(Line, Line0, Prev, T),pp_symbol(T)|pp_tokens(Ts, Line, T)];
pp_tokens([], _, _) -> [].

pp_symbol({var,_,Var}) -> atom_to_list(Var);
pp_symbol({_,_,Symbol}) -> io_lib:fwrite("~p", [Symbol]);
pp_symbol({dot, _}) -> ".";
pp_symbol({Symbol, _}) -> atom_to_list(Symbol).

pp_sep(Line, Line0, Prev, T) when Line > Line0 -> 
    ["\n    "|pp_sep(Line - 1, Line0, Prev, T)];
pp_sep(_, _, {'.',_}, _) -> "";        % No space after '.' (not a dot)
pp_sep(_, _, {'#',_}, _) -> "";        % No space after '#'
pp_sep(_, _, {'(',_}, _) -> "";        % No space after '('
pp_sep(_, _, {'[',_}, _) -> "";        % No space after '['
pp_sep(_, _, _, {'.',_}) -> "";        % No space before '.'
pp_sep(_, _, _, {'#',_}) -> "";        % No space before '#'
pp_sep(_, _, _, {',',_}) -> "";        % No space before ','
pp_sep(_, _, _, {')',_}) -> "";        % No space before ')'
pp_sep(_, _, _, {']',_}) -> "";        % No space before ']'
pp_sep(_, _, _, _) -> " ".

%% output_file_directive(File, Filename, Line) ->
%%     io:fwrite(File, <<"-file(~ts, ~w).\n">>,
%%               [format_filename(Filename, File), Line]).

%% format_filename(Filename0, File) ->
%%     Filename = filename:flatten(Filename0),
%%     case lists:keyfind(encoding, 1, io:getopts(File)) of
%%         {encoding, unicode} -> io_lib:write_string(Filename);
%%         _ ->                   io_lib:write_string_as_latin1(Filename)
%%     end.
