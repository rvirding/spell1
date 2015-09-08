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

##module

-export([format_error/1]).

%% The root symbol entry points
##export

%% User code. This is placed here to allow extra attributes.
##code

-record(spell1, {line=none,st=[],vs=[]}).     %Line, States, Values

%% parse1(Continuation, Tokens) ->
%%      {ok,Line,Sexpr,Rest} | {more,Continuation} | {error,Error,Rest}.
%%  This is the opt-level of the LL engine. It
%%  initialises/packs/unpacks the continuation information.

parse1([], Ts) ->                               %First call
    Start = start(),                            %The start state.
    parse1(#spell1{line=none,st=[Start],vs=[]}, Ts);
parse1(#spell1{line=none}=Lp, [T|_]=Ts) ->         %Guarantee a start line
    parse1(Lp#spell1{line=line(T)}, Ts);
parse1(#spell1{line=L,st=St0,vs=Vs0}, Ts) ->
    try
        parse2(Ts, St0, Vs0) of
        {done,Rest,[],[V]} -> {ok,L,V,Rest};
        {more,[],St1,Vs1} -> {more,#spell1{line=L,st=St1,vs=Vs1}};
        {error,Line,Error,Rest,_,_} ->
            %% Can't really continue from errors here.
            {error,{Line,?MODULE,Error},Rest}
    catch
        throw:{spell1_error,Error} ->
            {error,Error,[]}
    end.

%% parse2(Tokens, StateStack, ValueStack) ->
%%     {done,Ts,Sstack,Vstack} | {more,Ts,Sstack,Vstack} |
%%     {error,Line,Error,Ts,Sstack,Vstack}.
%%  Main loop of the parser engine. Handle any reductions on the top
%%  of the StateStack, then try to match type of next token with top
%%  state. If we have a match, it is a terminal, then push token onto
%%  value stack, else try to find new state(s) from table using
%%  current state and token type and push them onto state
%%  stack. Continue until no states left.

parse2(Ts, [{reduce,R}|St], Vs0) ->
    %% io:fwrite("p1: ~p\n", [{Ts,R,Vs0}]),
    %% Try to reduce values and push value on value stack.
    case reduce(R, Vs0) of
        {error,L,E} -> {error,L,E,Ts,St,Vs0};
        Vs1 -> parse2(Ts, St, Vs1)
    end;
parse2(Ts, [], Vs) -> {done,Ts,[],Vs};          %All done
parse2([T|Ts]=Ts0, [S|St]=St0, Vs) ->
    %% io:fwrite("p3: ~p\n", [{Ts0,St0,Vs}]),
    %% Try to match token type against state on stack.
    case type(T) of
        S -> parse2(Ts, St, [T|Vs]);                %Match
        Type ->                                     %Try to predict
            case table(S, Type) of
                error -> {error,line(T),{illegal,Type},Ts0,St0,Vs};
                Top -> parse2(Ts0, Top ++ St, Vs)
            end
    end;
parse2([], St, Vs) ->                           %Need more tokens
    {more,[],St,Vs};
parse2({eof,L}=Ts, St, Vs) ->                   %No more tokens
    {error,L,missing_token,Ts,St,Vs}.

%% Access the fields of a token.
-compile({nowarn_unused_function, type/1}).
-compile({nowarn_unused_function, line/1}).
-compile({nowarn_unused_function, value/1}).
type(T) -> element(1, T).
line(T) -> element(2, T).
value(T) -> element(3, T).

%% The root symbol entry points.
##entry

%% The table.
##table

%% The reductions, we are naive and straight forward here.
##reduce

%% format_error(Error) -> String.
%%  Format errors to printable string.

format_error(missing_token) -> "missing token";
format_error({illegal,What}) ->
    io_lib:fwrite("illegal ~p", [What]);
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        false -> io_lib:write(Message)
    end.

%% return_error(Error).
%%  To be used in grammar files to throw an error message to the
%%  parser toplevel. Doesn't have to be exported!

-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().

return_error(Line, Message) ->
    throw({spell1_error, {Line, ?MODULE, Message}}).
