;;; lfe-mode.el --- Lisp Flavoured Erlang mode

;; Copyright (c) 2012-2013 Robert Virding
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
;;;
;;; Copied from lisp-mode and scheme-mode and modified for LFE.

;;; Code:

(require 'lisp-mode)

(defvar lfe-mode-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    ;; Like scheme we allow [ ... ] as alternate parentheses.
    (modify-syntax-entry ?\[ "(]  " table)
    (modify-syntax-entry ?\] ")[  " table)
    table)
  "Syntax table in use in Lisp Flavoured Erlang mode buffers.")

;; (setq lfe-mode-syntax-table ())
;; (unless lfe-mode-syntax-table
;;   (setq lfe-mode-syntax-table (copy-syntax-table lisp-mode-syntax-table)))

(defvar lfe-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map "\e[" 'lfe-insert-brackets)
    map)
  "Keymap for Lisp Flavoured Erlang mode.")

;; (unless lfe-mode-map
;;   (setq lfe-mode-map (copy-keymap lisp-mode-map))
;;   (define-key lfe-mode-map "\e[" 'lfe-insert-brackets))

(defun lfe-insert-brackets (&optional arg)
  "Enclose following ARG sexps in brackets.
Leave point after open-bracket."
  (interactive "P")
  (insert-pair arg ?\[ ?\]))

(defvar lfe-mode-abbrev-table ()
  "Abbrev table used in Lisp Flavoured Erlang mode.")

(defvar lfe-mode-hook nil
  "*Hook for customizing Inferior LFE mode.")

;;;###autoload
(defun lfe-mode ()
  "Major mode for editing Lisp Flavoured Erlang. It's just like lisp mode.

Other commands:
\\{lfe-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'lfe-mode)
  (setq mode-name "LFE")
  (lfe-mode-variables)
  (use-local-map lfe-mode-map)
;;   ;; For making font-lock case independant, which LFE isn't.
;;   (make-local-variable 'font-lock-keywords-case-fold-search)
;;   (setq font-lock-keywords-case-fold-search t)
  (setq imenu-case-fold-search t)
  (run-mode-hooks 'lfe-mode-hook))

(defun lfe-mode-variables ()
  (set-syntax-table lfe-mode-syntax-table)
  (setq local-abbrev-table lfe-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat page-delimiter "\\|$" ))
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
  (setq comment-add 1)			;default to `;;' in comment-region
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'lisp-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
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
	  (font-lock-mark-block-function . mark-defun))))

;; Font locking

(defconst lfe-font-lock-keywords-1
  (eval-when-compile
    (list
     (list (concat "(\\(def\\("
		   ;; Base forms and old style names.
		   "\\(ine\\(-module\\|-function\\|-macro\\|"
		   "-syntax\\|-record\\)?\\)\\|"
		   ;; New model function names
		   "\\(un\\|macro\\|syntax\\)\\|"
		   ;; New model other names
		   "\\(module\\)\\|"
		   "\\(record\\)"
		   "\\)\\)\\>"
		   ;; Any whitespace and declared object.
		   "[ \t]*(?"
		   "\\(\\sw+\\)?")
	   '(1 font-lock-keyword-face)
	   '(8 (cond ((match-beginning 3) font-lock-function-name-face)
		     ((match-beginning 5) font-lock-function-name-face)
		     ((match-beginning 6) font-lock-variable-name-face)
		     (t font-lock-type-face))
	       nil t))
     ))
  "Subdued expressions to highlight in LFE modes.")

(eval-and-compile
  (defconst lfe-type-tests
    '("is_atom" "is_binary" "is_bitstring" "is_boolean" "is_float"
      "is_function" "is_integer" "is_list" "is_map" "is_number" "is_pid"
      "is_port" "is_record" "is_reference" "is_tuple")
    "LFE type tests")
  (defconst lfe-type-bifs
    '("abs" "bit_size" "byte_size" "element" "float"
      "hd" "iolist_size" "length" "make_ref" "setelement" ;"size"
      "round" "tl" "trunc" "tuple_size")
    "LFE builtin functions (BIFs)"))

(defconst lfe-font-lock-keywords-2
  (append lfe-font-lock-keywords-1
   (eval-when-compile
     (list
      ;; Control structures.
      (cons
       (concat
	"(" (regexp-opt
	     '(;; Core forms.
	       "cons" "car" "cdr" "list" "tuple" "binary"
	       "after" "call" "case" "catch"
	       "if" "lambda" "let" "let-function" "letrec-function"
	       "let-macro" "match-lambda"
	       "receive" "try" "funcall" "when" "progn"
	       "eval-when-compile"
	       ;; Default macros
	       "caar" "cadr" "cdar" "cddr"
	       "andalso" "cond" "do" "fun" "list*" "let*" "flet*" "macro"
	       "orelse" "syntax-rules" "lc" "bc" "flet" "fletrec"
	       "macrolet" "syntaxlet" "begin" "let-syntax"
	       ;; Should the map forms be here or as type bifs?
	       "map" "mref" "mset" "mupd" "map-get" "map-set" "map-update"
	       "match-spec" "qlc"
	       ":" "?" "++") t)
	"\\>") '(1 font-lock-keyword-face))
      ;; Type tests.
      (cons
       (concat
	"(" (regexp-opt (append lfe-type-tests lfe-type-bifs) t) "\\>")
       '(1 font-lock-builtin-face))
      )))
  "Gaudy expressions to highlight in LFE modes.")

(defvar lfe-font-lock-keywords lfe-font-lock-keywords-1
  "Default expressions to highlight in LFE modes.")

(defvar calculate-lisp-indent-last-sexp)

;; Copied from lisp-indent-function, but with gets of
;; lfe-indent-{function,hook}.
(defun lfe-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
					 calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(setq method (or (get (intern-soft function) 'lfe-indent-function)
			 (get (intern-soft function) 'lfe-indent-hook)))
	(cond ((or (eq method 'defun)
		   (and (null method)
			(> (length function) 3)
			(string-match "\\`def" function)))
	       (lisp-indent-defform state indent-point))
	      ((integerp method)
	       (lisp-indent-specform method state
				     indent-point normal-indent))
	      (method
		(funcall method state indent-point normal-indent)))))))


;; Special indentation rules. "def" anything is already fixed!

;; (put 'begin 'lfe-indent-function 0), say, causes begin to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

;; Old style forms.
(put 'begin 'lfe-indent-function 0)
(put 'let-syntax 'lfe-indent-function 1)
(put 'syntax-rules 'lfe-indent-function 0)
(put 'macro 'lfe-indent-function 0)
;; New style forms.
;; Core forms.
(put 'progn 'lfe-indent-function 0)
(put 'lambda 'lfe-indent-function 1)
(put 'match-lambda 'lfe-indent-function 0)
(put 'let 'lfe-indent-function 1)
(put 'let-function 'lfe-indent-function 1)
(put 'letrec-function 'lfe-indent-function 1)
(put 'let-macro 'lfe-indent-function 1)
(put 'if 'lfe-indent-function 1)
(put 'case 'lfe-indent-function 1)
(put 'receive 'lfe-indent-function 0)
(put 'catch 'lfe-indent-function 0)
(put 'try 'lfe-indent-function 1)
(put 'after 'lfe-indent-function 1)
(put 'call 'lfe-indent-function 2)
(put 'when 'lfe-indent-function 0)
(put 'eval-when-compile 'lfe-indent-function 0)
;; Core macros.
(put ': 'lfe-indent-function 2)
(put 'let* 'lfe-indent-function 1)
(put 'flet 'lfe-indent-function 1)
(put 'flet* 'lfe-indent-function 1)
(put 'fletrec 'lfe-indent-function 1)
(put 'macrolet 'lfe-indent-function 1)
(put 'syntaxlet 'lfe-indent-function 1)
(put 'do 'lfe-indent-function 2)
(put 'lc 'lfe-indent-function 1)
(put 'bc 'lfe-indent-function 1)
(put 'match-spec 'lfe-indent-function 0)

;;;###autoload
;; Associate ".lfe" with LFE mode.
(add-to-list 'auto-mode-alist '("\\.lfe\\'" . lfe-mode) t)

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
