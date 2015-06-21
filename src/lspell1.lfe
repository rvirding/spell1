;; Copyright (c) 2009-2015 Robert Virding
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; File    : lspell1.lfe
;; Author  : Robert Virding
;; Purpose : A simple LL(1) parser generator.

(defmodule lspell1
  (export all))

(defrecord spell1
  (base "")
  (gdir ".")                            ;Grammar directory
  (gfile "")                            ;Grammar file
  (odir ".")                            ;Output directory
  (lfile "")                            ;LFE file
  (ifile "")                            ;Include file
  (lfe-code 'none)                      ;LFE code starts here
  (module ())                           ;The module name
  (rules ())
  (gram 'none)
  (opts ())
  (errors ())
  (warnings ()))

;; Errors and warnings.
(defun format_error
  (['bad_declaration] "unknown or bad declaration"))

(defmacro DEFAULT-OPTS () `'(report verbose))
(defmacro INCLUDE-FILE () "spell1inc.lfe")

;; file(FileName [, Options]) -> Result.
;;  Generate a parser from the grammar defined in FileName.

(defun file (file) (file file (DEFAULT-OPTS)))

(defun file (file opts)
  (let* ((ifun (lambda ()
                 (let ((ret (try
                                (internal file opts)
                              (catch
                                ((tuple 'error reason _)
                                 (let ((st (erlang:get_stacktrace)))
                                   `#(error #(,reason ,st))))))))
                   (exit ret))))
         ((tuple pid ref) (spawn_monitor ifun)))
    (receive
      ((tuple DOWN r _ p res) (when (and (=:= r ref) (=:= p pid)))
       res))))

(defun internal (file opts)
  (let* ((st0 (make-spell1 gram (spell1_core:init_grammar opts)
                           opts opts))
         (st1 (filenames file st0)))
    (case (do-passes (passes) st1)
      ((tuple 'ok st2) (do-ok-return st2))
      ((tuple 'error st2) (do-error-return st2)))))

;; filenames(File, State) -> State.
;;  The default output dir is the current directory unless an
;;  explicit one has been given in the options.

(defun filenames (file st)
  ;; Test for explicit outdir.
  (let* ((odir (outdir (spell1-opts st)))
         (dir (filename:dirname file))
         (base (filename:basename file ".spell1"))
         (gfile (filename:join dir (++ base ".spell1")))
         (lfile (++ base ".lfe"))
         (ifile (includefile (spell1-opts st)))
         (module (list_to_atom base)))
    (set-spell1 st
                base base
                gdir dir
                gfile gfile
                odir odir
                lfile (filename:join odir lfile)
                ifile ifile
                module module)))

(defun outdir (opts) (find-opt 'outdir opts "."))
(defun includefile (opts) (find-opt 'includefile opts (INCLUDE-FILE)))

(defun find-opt
  ([opt (cons (tuple o val) _) def] (when (=:= opt o)) val)
  ([opt (cons (list o val) _) def] (when (=:= opt o)) val)
  ([opt (cons _ opts) def] (find-opt opt opts def))
  ([opt () def] def))                   ;The default
    
(defun do-passes
  ([(cons (tuple 'do fun) ps) st0]
   ;; (lfe_io:format "dp: ~p\n" (list (tuple fun st0)))
   (case (funcall fun st0)
     ((tuple 'ok st1) (do-passes ps st1))
     ((tuple 'error st1) (tuple 'error st1))))
  ([() st] (tuple 'ok st)))             ;Got to the end, everything ok

(defun passes ()
  (list (tuple 'do #'parse-grammar/1)
        (tuple 'do #'extract-grammar/1)
        (tuple 'do #'make-grammar/1)
        (tuple 'do #'output-file/1)
        (tuple 'do #'noop/1)))

(defmacro when-opt
  ((list* o os body)
   `(if (lists:member ,o ,os) (progn ,@body) 'ok)))

(defun do-ok-return (st)
  (let (((match-spell1 gfile gfile gram g opts opts warnings ws) st))
    (when-opt 'debug opts (spell1_core:dump_tables g))
    (when-opt 'report opts (list-warnings gfile ws))
    ;; Fix the right return.
    (case (lists:member 'return opts)
      ('true (tuple 'ok (return-errors gfile ws)))
      ('false 'ok))))

(defun do-error-return (st)
  (let (((match-spell1 gfile gfile opts opts errors es warnings ws) st))
    (when-opt 'report opts (list-errors gfile es))
    (when-opt 'report opts (list-warnings gfile ws))
    ;; Fix the right return.
    (case (lists:member 'return opts)
      ('true (tuple 'error (return-errors gfile es) (return-errors gfile ws)))
      ('false 'error))))

(defun return-errors
  ([_ ()] ())
  ([gfile es] (list (tuple gfile es))))

(defun list-warnings (f ws)
  (let ((wfun (match-lambda
                ([(tuple line mod warn)]
                 (let ((cs (call mod 'format_error warn)))
                   (lfe_io:format1 "~s:~w: Warning: ~s\n" (list f line cs)))))))
    (lists:foreach wfun ws)))

(defun list-errors (f es)
  (let ((efun (match-lambda
                ([(tuple line mod error)]
                 (let ((cs (call mod 'format_error error)))
                   (lfe_io:format1 "~s:~w: ~s\n" (list f line cs)))))))
    (lists:foreach efun es)))

;; add_error(Line, E, St) -> add_error(Line, ?MODULE, E, St).

(defun add-error (line mod e st)
  (add-error (tuple line mod e) st))

(defun add-error (et st)
  (set-spell1-errors st (++ (spell1-errors st) (list et))))

(defun add-warning (line w st)
  (add-warning line (MODULE) w st))

(defun add-warning (line mod w st)
  (let ((wt (tuple line mod w)))
    (set-spell1-warnings st (++ (spell1-warnings st) (list wt)))))

;; The passes.

(defun noop (st) (tuple 'ok st))        ;Dummy pass

;; parse_grammar(State) -> {ok,State} | {error,State}.
;;  Read in the grammar file. We keep reading till the end or until we
;;  read a line "lfe-code" when we store the line number and then
;;  we close the file.

(defun parse-grammar (st)
  (case (lfe_io:parse_file (spell1-gfile st))
    ((tuple 'ok forms)
     (parse-grammar forms st))
    ((tuple 'error e)
     (tuple 'error (add-error e st)))))

(defun parse-grammar
  ([(cons (tuple 'lfe-code line) _) st]
   (tuple 'ok (set-spell1-lfe-code st (+ line 1))))
  ([(cons f fs) st0]
   (let ((st1 (set-spell1-rules st0 (++ (spell1-rules st0) (list f)))))
     (parse-grammar fs st1)))
  ([() st] (tuple 'ok st)))

;; extract_grammar(State) -> {ok,State} | {error,State}.
;;  Extract the relevant g forms from the grammar file. For unknown
;;  declarations we just issue a warning then ignore them.

(defun extract-grammar (st0)
  (let* (((match-spell1 rules rs gram g0) st0)
         (efun (match-lambda
                 ([(tuple (list* 'rule name ss ts) l) (tuple g st)]
                  (tuple (spell1_core:add_rule l name ss ts g) st))
                 ([(tuple (cons 'terminals ts) l) (tuple g st)]
                  (tuple (spell1_core:add_terminals ts g) st))
                 ([(tuple (cons 'non-terminals nts) l) (tuple g st)]
                  (tuple (spell1_core:add_non_terminals nts g) st))
                 ([(tuple (list 'root-symbol root) l) (tuple g st)]
                  (tuple (spell1_core:add_root root g) st))
                 ([(tuple _ l) (tuple g st)]
                  (tuple g (add-warning l 'bad-declaration st)))))
         ((tuple g1 st1) (lists:foldl efun (tuple g0 st0) rs)))
    (tuple 'ok (set-spell1-gram st1 g1))))

;; make_grammar(State) -> {ok,State} | {error,State}.
;;  Make the grammar.

(defun make-grammar (st)
  (let (((match-spell1 gram g0 errors es warnings ws) st))
    (case (spell1_core:make_grammar g0)
      ((tuple 'ok g1 gws)
       (tuple 'ok (set-spell1 st gram g1 warnings (++ ws gws))))
      ((tuple 'error ges gws)
       (tuple 'error (set-spell1 st errors (++ es ges) warnings (++ ws gws)))))))

(defun output-file
  ([(= (match-spell1 lfile lfile ifile ifile) st0)]
   (let* (((tuple 'ok inc) (file:open ifile '(read)))
          ((tuple 'ok out) (file:open lfile '(write)))
          (st1 (output-file inc out st0 1)))
     (file:close inc)
     (file:close out)
     (if (=:= (spell1-errors st1) ())
       (tuple 'ok st1)
       (tuple 'error st1)))))

(defun output-file (inc out st l)
  (case (io:get_line inc 'spell1)
    ('eof st)
    ((tuple 'error e) (add-error e st))
    (line
     (case line
       (`(,@"##module" . ,_) (output-module out st))
       (`(,@"##code" . ,_) (output-user-code out st))
       (`(,@"##entry" . ,_) (output-entry out st))
       (`(,@"##table" . ,_) (output-table out st))
       (`(,@"##reduce" . ,_) (output-reduce out st))
       (_ (io:put_chars out line)))
     (output-file inc out st (+ l 1)))))

(defun output-module (out st)
  (let ((r (spell1_core:get_root (spell1-gram st))))
    (lfe_io:format out "(defmodule ~p\n" (list (spell1-module st)))
    (lfe_io:format out "  (export (~p 1) (~p 2)))\n" (list r r))))

(defun output-user-code
  ([_ (match-spell1 lfe-code 'none)] 'ok)
  ([out (match-spell1 gfile gfile lfe-code eline)]
   (let (((tuple 'ok in) (file:open gfile '(read))))
     (skip-lines in eline)
     (output-lines in out)
     (file:close in))))

(defun skip-lines
  ([in l] (when (> l 0))
   (case (io:get_line in 'spell1)
     ('eof 'ok)
     ((tuple 'error _) 'ok)
     (_ (skip-lines in (- l 1)))))      ;A line
  ([in 0] 'ok))

(defun output-lines (in out)
  (case (io:get_line in 'spell1)
    ('eof 'ok)
    ((tuple 'error _) 'ok)
    (line
     (io:put_chars out line)
     (output-lines in out))))

(defun output-entry (out st)
  (let ((r (spell1_core:get_root (spell1-gram st))))
    (lfe_io:format out "(defun ~p (ts) (parse1 () ts))\n" (list r))
    (lfe_io:format out "(defun ~p (cont ts) (parse1 cont ts))\n" (list r))))

(defun output-table (out st)
  (let* ((g (spell1-gram st))
         (r (spell1_core:get_root g)))
    ;; Define the start state of the grammar.
    (lfe_io:format out "(defun start () '~p)\n\n" (list r))
    ;; Now output the table function.
    (io:put_chars out "(defun table\n")
    (let ((rfun (lambda (nt t body)
                  (lfe_io:format out "  (['~w '~w] '~w)\n" (list nt t body)))))
      (spell1_core:output_table rfun g)
      (io:put_chars out "  ([_ _] 'error))\n"))))

(defun output-reduce (out st)
  (let* ((g (spell1-gram st))
         (rfun (lambda (n symlen toks) (output-reduce-row out n symlen toks))))
    (io:put_chars out "(defun reduce\n")
    (spell1_core:output_reduce rfun g)
    (io:put_chars out "  ([_ _] (error 'function_clause)))\n")))

;; output-reduce-clause(Out, N, Symlen, Tokens) -> ok.
;;  Output one row/clause of the reduce function.

(defun output-reduce-row
  ([out n 0 toks]
   ;; EPSILON rules are special in that they don't consume any tokens!
   (lfe_io:format out "  ([~w $vs] (cons ~p $vs))\n"
                  (list n (cons 'progn toks))))
  ([out n symlen toks]
;;   (let ((stpat (lists:sublist '($9 $8 $7 $6 $5 $4 $3 $2 $1)
;;                             (- 10 symlen) 9)))
   (let ((stpat (pat-list symlen)))
     (lfe_io:format out "  ([~w ~w] (cons ~p $vs))\n"
                    (list n `(list* ,@stpat $vs) `(progn ,@toks))))))

(defun pat-list
  ([0] ())
  ([n] (when (> n 0))
   (cons (list_to_atom (cons #\$ (integer_to_list n)))
         (pat-list (- n 1)))))
