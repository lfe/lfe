;;; lfe-mode.el --- Lisp Flavoured Erlang mode

;; Copyright (c) 2012-2015 Robert Virding
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

;;; Author Robert Virding

;;; Commentary:
;; Copied from `lisp-mode' and modified for LFE.

;;; Code:

(require 'lisp-mode)

(defgroup lfe nil
  "LFE support."
  :group 'lisp
  :group 'languages)

(defvar prettify-symbols-alist ())

(defconst lfe--prettify-symbols-alist '(("lambda"  . ?λ))
  "Prettfy symbols alist user in Lisp Flavoured Erlang mode.")

(defvar lfe-mode-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    ;; Like scheme we allow [ ... ] as alternate parentheses.
    (modify-syntax-entry ?\[ "(]  " table)
    (modify-syntax-entry ?\] ")[  " table)
    table)
  "Syntax table in use in Lisp Flavoured Erlang mode buffers.")

(defvar lfe-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map "\e[" 'lfe-insert-brackets)
    map)
  "Keymap for Lisp Flavoured Erlang mode.")

(defvar lfe-mode-abbrev-table ()
  "Abbrev table used in Lisp Flavoured Erlang mode.")

(defvar lfe-mode-hook nil
  "*Hook for customizing Inferior LFE mode.")

(defun lfe-insert-brackets (&optional arg)
  "Enclose following `ARG' sexps in brackets.
Leave point after open-bracket."
  (interactive "P")
  (insert-pair arg ?\[ ?\]))

;;;###autoload
(defun lfe-mode ()
  "Major mode for editing Lisp Flavoured Erlang.  It's just like `lisp-mode'.

Other commands:
\\{lfe-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'lfe-mode)
  (setq mode-name "LFE")
  (lfe-mode-variables)
  (use-local-map lfe-mode-map)
  ;;   ;; For making font-lock case independent, which LFE isn't.
  ;;   (make-local-variable 'font-lock-keywords-case-fold-search)
  ;;   (setq font-lock-keywords-case-fold-search t)
  (setq imenu-case-fold-search t)
  (run-mode-hooks 'lfe-mode-hook))

(defun lfe-mode-variables ()
  "Variables for LFE modes."
  (set-syntax-table lfe-mode-syntax-table)
  (setq local-abbrev-table lfe-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat page-delimiter "\\|$"))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)
  (make-local-variable 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'lisp-mode-auto-fill)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lisp-indent-line)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp ";;;;* \\|(")
  (make-local-variable 'outline-level)
  (setq outline-level 'lisp-outline-level)
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  (make-local-variable 'comment-add)
  (setq comment-add 1)                  ;default to `;;' in comment-region
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'lisp-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  ;; Make lisp-indent-line call lfe-indent-line.
  (make-local-variable 'lisp-indent-function)
  (set lisp-indent-function 'lfe-indent-function)
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression lisp-imenu-generic-expression)
  (make-local-variable 'multibyte-syntax-as-symbol)
  (setq multibyte-syntax-as-symbol t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((lfe-font-lock-keywords
           lfe-font-lock-keywords-1 lfe-font-lock-keywords-2)
          nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) beginning-of-defun
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . lisp-font-lock-syntactic-face-function)))
  ;; Don't use seq-local here for backwards compatibility.
  (make-local-variable 'prettify-symbols-alist)
  (setq prettify-symbols-alist lfe--prettify-symbols-alist))

;;; Font locking

(defconst lfe-font-lock-old-type-keywords
  (eval-when-compile
    (list
     (concat
      "(\\(define-\\(module\\|record\\)\\)\\>"
      ;; Any whitespace and declared object.
      "[ \t]*(?"
      "\\(\\sw+\\)?")
     '(1 font-lock-keyword-face)
     '(3 font-lock-type-face nil t)))
  "LFE old type definition forms.")

(defconst lfe-font-lock-old-function-keywords
  (eval-when-compile
    (list
     (concat
      "(\\(define\\(-function\\|-macro\\|-syntax\\)?\\)\\>"
      ;; Any whitespace and declared object.
      "[ \t]*(?"
      "\\(\\sw+\\)?")
     '(1 font-lock-keyword-face)
     '(3 font-lock-function-name-face nil t)))
  "LFE old function definition forms.")

(defconst lfe-font-lock-type-keywords
  (eval-when-compile
    (list
     (concat
      "(\\(def\\(module\\|record\\)\\)\\>"
      ;; Any whitespace and declared object.
      "[ \t]*(?"
      "\\(\\sw+\\)?")
     '(1 font-lock-keyword-face)
     '(3 font-lock-type-face nil t)))
  "LFE type definition forms.")

(defconst lfe-font-lock-function-keywords
  (eval-when-compile
    (list
     (concat
      "(\\(def\\(un\\|macro\\|syntax\\|method\\)\\)\\>"
      ;; Any whitespace and declared object.
      "[ \t]*(?"
      "\\(\\sw+\\)?")
     '(1 font-lock-keyword-face)
     '(3 font-lock-function-name-face nil t)))
  "LFE function definition forms.")

(defconst lfe-font-lock-test-keywords
  (eval-when-compile
    (list
     (concat
      ;; No method here!
      "(\\(def\\(\\(test\\(gen\\|skip\\|case\\|cases\\)?\\)\\|\\(setup\\|teardown\\)\\)\\)\\>"
      ;; Any whitespace and declared object.
      "[ \t]*(?"
      "\\(\\sw+\\)?")
     '(1 font-lock-keyword-face)
     '(6 font-lock-function-name-face nil t)))
  "LFE test definition forms.")

(defconst lfe-font-lock-flavor-keywords
  (eval-when-compile
    (list
     (concat
      ;; No method here!
      "(\\(defflavor\\|endflavor\\)\\>"
      ;; Any whitespace and declared object.
      "[ \t]*(?"
      "\\(\\sw+\\)?")
     '(1 font-lock-keyword-face)
     '(2 font-lock-type-face nil t)))
  "LFE flavor definition forms.")

(defconst lfe-font-lock-keywords-1
  (eval-when-compile
    (list lfe-font-lock-type-keywords
          lfe-font-lock-function-keywords
          lfe-font-lock-test-keywords
          lfe-font-lock-flavor-keywords
          lfe-font-lock-old-type-keywords
          lfe-font-lock-old-function-keywords))
  "Subdued expressions to highlight in LFE modes.")

(eval-and-compile
  (defconst lfe-type-tests
    '("is_atom" "is_binary" "is_bitstring" "is_boolean" "is_float"
      "is_function" "is_integer" "is_list" "is_map" "is_number" "is_pid"
      "is_port" "is_record" "is_reference" "is_tuple")
    "LFE type tests")
  (defconst lfe-type-forms
    '("abs" "bit_size" "byte_size" "element" "float"
      "hd" "iolist_size" "length" "make_ref" "setelement" ;"size"
      "round" "tl" "trunc" "tuple_size"
      "car" "cdr" "caar" "cadr" "cdar" "cddr"
      ;; Just for the fun of it.
      "caaar" "caadr" "cadar" "caddr" "cdaar" "cddar" "cdadr" "cdddr"
      "function" "list" "list*" "tuple" "binary"
      "map" "mref" "mset" "mupd" "map-get" "map-set" "map-update")
    "LFE builtin functions (BIFs) and some type macros")
  (defconst lfe-core-forms
    '(
      ;; Core forms.
      "after" "call" "case" "catch" "define-function" "define-macro"
      "funcall" "if" "lambda"
      "let" "let-function" "letrec-function" "let-macro"
      "match-lambda" "progn" "receive" "try" "when"
      "eval-when-compile" "extend-module"
      ;; Core macro forms.
      "andalso" "bc" "binary-comp" "cond" "do" "flet" "flet*" "fletrec"
      "fun" "lc" "list-comp"
      "let*" "match-spec" "macrolet" "orelse" "qlc"
      ":" "?" "++")
    "LFE basic forms"))

(defconst lfe-font-lock-keywords-2
  (append
   lfe-font-lock-keywords-1
   (eval-when-compile
     (list
      ;; Control structures.
      (cons
       (concat
        "(" (regexp-opt lfe-core-forms t) "\\>")
       '(1 font-lock-keyword-face))
      ;; Type tests.
      (cons
       (concat
        "(" (regexp-opt (append lfe-type-tests lfe-type-forms) t) "\\>")
       '(1 font-lock-builtin-face)))))
  "Gaudy expressions to highlight in LFE modes.")

(defvar lfe-font-lock-keywords lfe-font-lock-keywords-1
  "Default expressions to highlight in LFE modes.")


;;;###autoload
;; Associate ".lfe{s,sh}?" with LFE mode.
(add-to-list 'auto-mode-alist '("\\.lfe\\(s\\|sh\\)?\\'" . lfe-mode) t)

;;;###autoload
;; Ignore files ending in ".jam", ".vee", and ".beam" when performing
;; file completion.
(dolist (lfe-ext '(".beam" ".jam" ".vee"))
  (add-to-list 'completion-ignored-extensions lfe-ext))

;; The end.
(provide 'lfe-mode)

(defvar lfe-load-hook nil
  "*Functions to run when LFE mode is loaded.")

(run-hooks 'lfe-load-hook)
;;; lfe-mode.el ends here
