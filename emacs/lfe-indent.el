;;; lfe-indent.el --- Lisp Flavoured Erlang indent mode

;; Copyright (c) 2015 Robert Virding
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
(require 'lfe-mode)

;;; Lisp indent

(defvar calculate-lisp-indent-last-sexp)

;;;###autoload
(defun lfe-indent-function (indent-point state)
  "This function is the normal value of the variable `lfe-indent-function'.

If this function is the value of the variable `lisp-indent-function' then
`calculate-lisp-indent' will call it to determine if the
arguments of a LFE function call should be indented specially.

INDENT-POINT is the position where the user typed TAB, or equivalent.
Point is located at the point to indent under;
`STATE' is the `parse-partial-sexp' state for that position.

Copied from function `lisp-indent-function', but with gets of
lfe-indent-{function,hook} and it uses `lfe-body-indent'."
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
	;; Don't use function-get here for backwards compatibility.
        (setq method (or (get (intern-soft function) 'lfe-indent-function)
                         (get (intern-soft function) 'lfe-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lfe-indent-defform state indent-point))
              ((integerp method)
               (lfe-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method state indent-point normal-indent)))))))

(defcustom lfe-body-indent 2
  "Number of columns to indent the second line of a `(def...)' form."
  :group 'lfe
  :type 'integer)
(put 'lfe-body-indent 'safe-local-variable 'integerp)

(defun lfe-indent-specform (count state indent-point normal-indent)
  (let ((containing-form-start (elt state 1))
        (i count)
        body-indent containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count), and move past the
    ;; function symbol.  lfe-indent-function guarantees that there is at
    ;; least one word or symbol character following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (setq body-indent (+ lfe-body-indent containing-form-column))
    (forward-char 1)
    (forward-sexp 1)
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
                (condition-case ()
                    (progn
                      (setq count (1- count))
                      (forward-sexp 1)
                      (parse-partial-sexp (point) indent-point 1 t))
                  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (if (> count 0)
        ;; A distinguished form.  If it is the first or second form use double
        ;; lfe-body-indent, else normal indent.  With lfe-body-indent bound
        ;; to 2 (the default), this just happens to work the same with if as
        ;; the older code, but it makes unwind-protect, condition-case,
        ;; with-output-to-temp-buffer, et. al. much more tasteful.  The older,
        ;; less hacked, behavior can be obtained by replacing below with
        ;; (list normal-indent containing-form-start).
        (if (<= (- i count) 1)
            (list (+ containing-form-column (* 2 lfe-body-indent))
                  containing-form-start)
            (list normal-indent containing-form-start))
      ;; A non-distinguished form.  Use body-indent if there are no
      ;; distinguished forms and this is the first undistinguished form,
      ;; or if this is the first undistinguished form and the preceding
      ;; distinguished form has indentation at least as great as body-indent.
      (if (or (and (= i 0) (= count 0))
              (and (= count 0) (<= body-indent normal-indent)))
          body-indent
          normal-indent))))

(defun lfe-indent-defform (state indent-point)
  (goto-char (car (cdr state)))
  (forward-line 1)
  (if (> (point) (car (cdr (cdr state))))
      (progn
	(goto-char (car (cdr state)))
	(+ lfe-body-indent (current-column)))))

;;; Indentation rule helpers
;; Modified from `clojure-mode'.

(defun put-lfe-indent (sym indent)
  "Instruct `lfe-indent-function' to indent the body of `SYM' by `INDENT'."
  (put sym 'lfe-indent-function indent))

(defmacro define-lfe-indent (&rest kvs)
  "Call `put-lfe-indent' on a series, `KVS'."
  `(progn
     ,@(mapcar (lambda (x)
                 `(put-lfe-indent (quote ,(car x)) ,(cadr x)))
               kvs)))

;;; Special indentation rules
;; "def" anything is already fixed!

;; (define-lfe-indent (begin 0)), say, causes begin to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

(define-lfe-indent
  (: 2)
  (after 1)
  (bc 1)
  (binary-comp 1)
  (call 2)
  (case 1)
  (catch 0)
  (define-function 1)
  (define-macro 1)
  (define-module 1)
  (extend-module 0)
  (do 2)
  (eval-when-compile 0)
  (flet 1)
  (flet* 1)
  (fletrec 1)
  (if 1)
  (lambda 1)
  (let 1)
  (let* 1)
  (let-function 1)
  (letrec-function 1)
  (let-macro 1)
  (lc 1)
  (list-comp 1)
  (macrolet 1)
  (match-lambda 0)
  (match-spec 0)
  (prog1 1)
  (prog2 2)
  (progn 0)
  (receive 0)
  (try 1)
  (when 0)
  (syntaxlet 1)

  (defflavor 3)				;This doesn't behave like other def's

  ;; Old style forms.
  (begin 0)
  (let-syntax 1)
  (syntax-rules 0)
  (macro 0))

(provide 'lfe-indent)
;;; lfe-indent.el ends here
