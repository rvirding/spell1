# spell1
LL(1) parser generator for Erlang and LFE

This is a basic LL(1) parser generator for use together with
Erlang. There are two interfaces for different languages, Erlang and
LFE, and others can be added. The generator has been split into
separate files for handling the grammar and the reading of grammar
files and outputing the resultant module code. The grammar is
processed in the file `spell1_core.erl` and the IO-files for Erlang
and LFE are inte `spell1.erl` and `lspell1.lfe`. It is easy to add
IO-files for other languages.

## Using the generated parser

The parser which spell1 generates contains by default two exported
functions. They have the same name as the root symbol (see below) and
are the default entry into the parser. They have the type:

    <root symbol>(Tokens) -> Return.
    <root symbol>(Continuation, Tokens) -> Return.
    
        Types:
        
            Tokens = [Token]
            Continuation = [] | continuation()
            Token = {Type,LINE} | {Type,LINE,Value}
            Return = {ok,Form,RestTokens} | {more,Continuation} |
                     {error,Error,RestTokens}

In a token: `Type` is one of the terminals; `LINE` is some form of metadata
about the token usually containing the line number; and `Value` gives
a specific value for that terminal, for example the actual number or
atom.

The functions returns return any remaining tokens which weren't used
in the form, these can then be used to parse the next form. When the
functions return `{more,Continuation}` they don't have enough tokens
to parse a form and the function can be called again with the
continuation and more tokens to continue parsing. This can be repeated
until enough tokens have been received and the form is parsed.

## Grammar definition files

The grammar definitions are in files with the default extension
`.spell1`. A grammar file must: list all the terminal and non-terminal
symbols; indicate the root symbol; give all the grammar rules; and
give any extra code. The default function for parsing has the same
name as the root symbol.

### Erlang

The syntax of the Erlang grammar file is the same as for yecc but only
the following declarations are recognised: `Terminals`, `Nonterminals`
and `Rootsymbol`. Other declarations generate a warning and are
ignored. Rules take the same format as in yecc.

### LFE

The syntax is of the LFE grammar file recognises the following forms:

    (terminals terminal-1 terminal-2 ...)
    (non-terminals non-terminal-1 non-terminal-2 ...)
    (root-symbol root)
    (rule non-term (symbol-1 ...) code)

An optional section containing LFE code starts with the declaration `lfe-code`.

## Example grammar files

Here is the grammar file for LFE both as an Erlang version and as an
LFE version.

### Erlang

```erlang
Terminals
    symbol number string binary fun '(' ')' '[' ']' '.' '\'' '`' ',' ',@'
    '#(' '#B(' '#M(' '#\''.

Nonterminals form sexpr list list_tail proper_list .

Rootsymbol form.

form -> sexpr : '$1'.
sexpr -> symbol : value('$1').
sexpr -> number : value('$1').
sexpr -> string : value('$1').
sexpr -> binary : value('$1').
sexpr -> '#\'' : make_fun(value('$1')).
sexpr -> '\'' sexpr : [quote,'$2'].
sexpr -> '`' sexpr : [backquote,'$2'].
sexpr -> ',' sexpr : [comma,'$2'].
sexpr -> ',@' sexpr : ['comma-at','$2'].
sexpr -> '(' list ')' : '$2'.
sexpr -> '[' list ']' : '$2'.
sexpr -> '#(' proper_list ')' : list_to_tuple('$2').
sexpr -> '#B(' proper_list ')' :
        make_bin(line('$1'), '$2').
sexpr -> '#M(' proper_list ')' :
        make_map(line('$1'), '$2').
list -> sexpr list_tail : ['$1'|'$2'].
list -> '$empty' : [].
list_tail -> sexpr list_tail : ['$1'|'$2'].
list_tail -> '.' sexpr : '$2'.
list_tail -> '$empty' : [].
proper_list -> sexpr proper_list : ['$1'|'$2'].
proper_list -> '$empty' : [].

%% Extra Erlang code.
Erlang code.

%% For backwards compatibility
-export([sexpr/1,sexpr/2]).

sexpr(Ts) -> form(Ts).
sexpr(Cont, Ts) -> form(Cont, Ts).

%% make_fun(String) -> FunList.
%%  Convert a fun string to a fun sexpr.
%%    "F/A" -> ['fun', F, A].
%%    "M:F/A" -> ['fun', M, F, A].

make_fun("=:=/2") ->
    ['fun', '=:=', 2];
make_fun(FunStr) ->
    J = string:rchr(FunStr, $/),
    A = list_to_integer(string:substr(FunStr, J + 1)),
    case string:chr(FunStr, $:) of
        0 ->
            F = list_to_atom(string:substr(FunStr, 1, J - 1)),
            ['fun', F, A];
        I ->
            F = list_to_atom(string:substr(FunStr, I + 1, J - I - 1)),
            M = list_to_atom(string:substr(FunStr, 1, I - 1)),
            ['fun', M, F, A]
    end.

%% make_bin(Line, Segments) -> Binary.
%%  Make a binary from the segments.

make_bin(Line, Segs) ->
    case catch lfe_eval:expr([binary|Segs]) of
        Bin when is_bitstring(Bin) -> Bin;
        _ -> return_error(Line, "bad binary")
    end.

%% make_map(Line, Elements) -> Map.
%%  Make a map from the key/value elements.

make_map(Line, Es) ->
    case catch maps:from_list(pair_list(Es)) of
        Map when is_map(Map) -> Map;
        _ -> return_error(Line, "bad map")
    end.

%% pair_list(List) -> [{A,B}].
%%  Generate a list of tuple pairs from the elements. An error if odd
%%  number of elements in list.

pair_list([A,B|L]) -> [{A,B}|pair_list(L)];
pair_list([]) -> [].
```

### LFE

```lisp
(terminals symbol number string binary fun |(| |)| |[| |]| |.| |'| |`| |,| |,@|
           |#(| |#B(| |#M(| |#'| )

(non-terminals form sexpr list list-tail proper-list )

(root-symbol form)

(rule form (sexpr) $1)
(rule sexpr (symbol) (value $1))
(rule sexpr (number) (value $1))
(rule sexpr (string) (value $1))
(rule sexpr (binary) (value $1))
(rule sexpr (|#'|) (make-fun (value $1)))
(rule sexpr (|'| sexpr) `(quote ,$2))
(rule sexpr (|`| sexpr) `(backquote ,$2))
(rule sexpr (|,| sexpr) `(comma ,$2))
(rule sexpr (|,@| sexpr) `(comma-at ,$2))
(rule sexpr (|(| list |)|) $2)
(rule sexpr (|[| list |]|) $2)
(rule sexpr (|#(| proper-list |)|) (list_to_tuple $2))
(rule sexpr (|#B(| proper-list |)|)
      (make-bin (line $1) $2))
(rule sexpr (|#M(| proper-list |)|)
      (make-map (line $1) $2))
(rule list (sexpr list-tail) (cons $1 $2))
(rule list ($empty) ())
(rule list-tail (sexpr list-tail) (cons $1 $2))
(rule list-tail (|.| sexpr) $2)
(rule list-tail ($empty) ())
(rule proper-list (sexpr proper-list) (cons $1 $2))
(rule proper-list ($empty) ())

;; Extra LFE code.
lfe-code

;; For backwards compatibility
(extend-module (export (sexpr 1) (sexpr 2)))

(defun sexpr (ts) (form ts))
(defun sexpr (cont ts) (form cont ts))

;; make_fun(String) -> FunList.
;;  Convert a fun string to a fun sexpr.
;;    "F/A" -> ['fun', F, A].
;;    "M:F/A" -> ['fun', M, F, A].

(defun make-fun
  (["=:=/2"] `(fun =:= 2))
  ([str]
   (let* ((j (string:rchr str #\/))
          (a (list_to_integer (string:substr str 1 (+ j 1)))))
     (case (string:chr str #\:)
       (0 (let ((f (list_to_atom (string:substr str 1 (- j 1)))))
            `(fun ,f ,a)))
       (i (let ((f (list_to_atom (string:substr str (+ i 1) (- j i 1))))
                (m (list_to_atom (string:substr str 1 (- i 1)))))
            `(fun ,m ,f ,a)))))))

;; make-bin(Line, Segments) -> Binary.
;;  Make a binary from the segments.

(defun make-bin (line segs)
  (case (catch (lfe_eval:expr (cons 'binary segs)))
    (bin (when (is_bitstring bin)) bin)
    (_ (return_error line "bad binary"))))

;; make-map(Line, Elements) -> Map.
;;  Make a map from the key/value elements.

(defun make-map (line es)
  (case (catch (maps:from_list (pair-list es)))
    (map (when (is_map map)) map)
    (_ (return_error line "bad map"))))

;; pair_list(List) -> [{A,B}].
;;  Generate a list of tuple pairs from the elements. An error if odd
;;  number of elements in list.

(defun pair-list
  ([`(,a ,b . ,l)] `(#(,a ,b) . ,(pair-list l)))
  ([()] ()))
```
