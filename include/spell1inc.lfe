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

;; The module definition and exports.
##module

;; User code. This is placed here to allow extra attributes.
##code

(defrecord spell1 (line 'none) (st ()) (vs ()))

;; parse1(Continuation, Tokens) ->
;;      #(ok line sexpr rest) | #(more continuation) | #(error error rest).
;;  This is the opt-level of the LL engine. It
;;  initialises/packs/unpacks the continuation information.

(defun parse1
  ([() ts]                              ;First call
   (let ((start (start)))
     (parse1 (make-spell1 line 'none st (list start) vs ()) ts)))
  ([(= (match-spell1 line 'none) lp) (= (cons t _) ts)]
   (parse1 (set-spell1-line lp (line t)) ts))
  ([(match-spell1 line l st st0 vs vs0) ts]
    (try
        (parse2 ts st0 vs0)
      (case
          (`#(done ,rest () (,v)) `#(ok ,l ,v ,rest))
          (`#(more () ,st1 ,vs1)
           `#(more ,(make-spell1 line l st st1 vs vs1)))
          (`#(error ,line ,error ,rest ,_ ,_)
           ;; Can't really continue from errors here.
           `(error #(,line ,(MODULE) ,error) ,rest)))
      (catch
        (`#(throw #(spell1-error ,error) ,_)    ;User error
         `#(error ,error ()))))))

;; parse2(Tokens, StateStack, ValueStack) ->
;;     #(done ts s-stack v-stack) | #(more ts s-stack v-stack) |
;;     #(error line error ts s-stack v-stack).
;;  Main loop of the parser engine. Handle any reductions on the top
;;  of the StateStack, then try to match type of next token with top
;;  state. If we have a match, it is a terminal, then push token onto
;;  value stack, else try to find new state(s) from table using
;;  current state and token type and push them onto state
;;  stack. Continue until no states left.

(defun parse2
  ([ts `(#(reduce ,r) . ,st) vs0]
   ;; Try to reduce values and push value on value stack.
   (case (reduce r vs0)
     (`#(error ,l ,e) `#(error ,l ,e ,ts ,st ,vs0))
     (vs1 (parse2 ts st vs1))))
  ([ts () vs]
   `#(done ,ts () ,vs))
  ([(= (cons t ts) ts0) (= (cons s st) st0) vs]
   ;; Try to match token type against state on stack.
   (let ((tt (type t)))
     (if (=:= tt s)
       (parse2 ts st (cons t vs))
       (case (table s tt)
         ('error `#(error ,(line t) #(illegal ,tt) ,ts0 ,st0 ,vs))
         (top (parse2 ts0 (++ top st) vs))))))
  ([() st vs]
   (tuple 'more () st vs))
  ([(= ts `#(eof ,l)) st vs]
   `#(error ,l missing_token ,ts ,st ,vs)))

;; Access the fields of a token.
(defun type (t) (element 1 t))
(defun line (t) (element 2 t))
(defun value (t) (element 3 t))

;; The root symbol entry points.
##entry

;; The table.
##table

;; The reductions, we are naive and straight forward here.
##reduce

;; format_error(Error) -> String.
;;  Format errors to printable string.

(defun format_error
  (['missing_token] "missing token")
  ([`#(illegal ,what)]
   (lfe_io:format1 "illegal ~p" (list what)))
  ([message]
   (if (io_lib:deep_char_list message)
     message
     (lfe_io:print1 message))))

(defun return_error (line message)
  (throw `#(spell1-error #(,line ,(MODULE) ,message))))
