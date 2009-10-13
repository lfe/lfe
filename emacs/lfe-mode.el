;;; lfe-mode.el --- Lisp Flavoured Erlang mode
;;; Author Robert Virding
;;;
;;; Copied from lisp-mode and scheme-mode and modified for LFE.

;;; Code:

(require 'lisp-mode)

(defvar lfe-mode-map ()
  "*Keymap used in Lisp Flavoured Erlang mode.")

(unless lfe-mode-map
  (setq lfe-mode-map (copy-keymap lisp-mode-map)))

(defvar lfe-mode-abbrev-table ()
  "Abbrev table used in Lisp Flavoured Erlang mode.")

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

(define-derived-mode lfe-mode nil "LFE"
  "Major mode for editing Lisp Flavoured Erlang. It's just like lisp mode."
  (use-local-map lfe-mode-map)
  (lfe-mode-variables)
;;   ;; For making font-lock case independant, which LFE isn't.
;;   (make-local-variable 'font-lock-keywords-case-fold-search)
;;   (setq font-lock-keywords-case-fold-search t)
  (setq imenu-case-fold-search t))

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
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'lisp-indent-region)
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
		   ;; Base forms and old model names.
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

(defconst lfe-font-lock-keywords-2
  (append lfe-font-lock-keywords-1
   (eval-when-compile
     (list
      ;;
      ;; Control structures.
      (cons
       (concat
	"(" (regexp-opt
	     '(;; Core forms.
	       "cons" "car" "cdr" "list" "tuple" "binary"
	       "after" "call" "case" "catch"
	       "if" "lambda" "let" "let-function" "letrec-function"
	       "let-macro" "match-lambda"
	       "receive" "try" "when" "progn"
	       "eval-when-compile"
	       ;; Default macros
	       "andalso" "cond" "do" "fun" "list*" "let*" "flet*" "macro"
	       "orelse" "syntax-rules" "lc" "bc" "flet" "fletrec"
	       "macrolet" "syntaxlet" "begin" "let-syntax"
	       ":" "?" "++") t)
	"\\>") '(1 font-lock-keyword-face))
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
(put 'try 'lfe-indent-function 0)
(put 'after 'lfe-indent-function 0)
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

;; The end.
(provide 'lfe-mode)

(run-hooks 'lfe-load-hook)
