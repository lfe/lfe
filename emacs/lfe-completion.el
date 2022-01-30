;;; lfe-completion.el --- Code for finding LFE documentation and auto-completion

;; Copyright (c) 2017 Jacek Podkanski
;; Copyright (c) 2022 Manfred Bergmann
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

;; Original author Jacek Podkanski
;; Modifications and refactorings by Manfred Bergmann in 2022

;;; Commentary:

;;; Problems and possible improvements:
;;; - as backend 'lfe' process is called, this is not really efficient.
;;; This should at some point be a server process.
;;; However, then a standardised LSP implementation might be a better solution.
;;; eglot or some other LSP client can then be used. So I'd not wanna venture too far into a custom server.
;;; - using eldoc to show documentation popups. But then again, this got to be simple, because LSP has all that.

;;; Usage:

;;; Load this file by adding it to the load-path and running:
;;; (require 'lfe-completion)
;;; or
;;; (use-package lfe-completion)
;;;
;;; Completion works automatically after entering 2 characters.
;;; When no ":" is part of the entered string then the logic searches for modules and symbols.
;;; I.e. when entering "(de)" will show completions for modules and symbols that start with "de".
;;; When string contains ":" (denoting a function/symbol of a mobule) then functions of this particular module are searched and presented as completion options.
;;; I.e. when entering "(io:fo)" completions for functions starting with "fo" of module "io" are presented.


;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'company)

(global-set-key (kbd "s-1") 'company-lfe-backend)
(global-set-key (kbd "s-7") 'lfeac--module-functions) ; with arity

;;; -------------- company -------------------

(defun company-lfe-backend (command &optional arg &rest ignored)
  "Get auto completion COMMAND for ARG and IGNORED."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-lfe-backend))
    (prefix (and (or (eq major-mode 'lfe-mode)
                     (eq major-mode 'inferior-lfe-mode))
                 (company-grab-symbol)))
    (candidates
     (lfeac-new-ac-at-point arg))))

(add-to-list 'company-backends 'company-lfe-backend)

;;; ------------- auto-completion -----------------

(defun lfeac-new-ac-at-point (arg)
  (interactive)
  "Get ne auto completions at point."  
  (lfeac-new-autocompletions arg (current-buffer) (point-marker)))

(defun lfeac-new-autocompletions (arg buffer marker)
  "New auto completion for ARG in BUFFER at MARKER."
  (let ((se (lfeac--sexp-at-point buffer marker))
        (ss (split-string arg ":")))
    (if (equal 1 (length ss))
        (lfeac-ac-symbols-and-modules arg)
      (lfeac-ac-module-functions (car ss) (cadr ss)))))

(defun lfeac-ac-symbols-and-modules (arg)
  (cl-remove-if-not
   (lambda (c) (string-prefix-p arg c))
   (-map (lambda (x) (format "%s" x))
         (-flatten (list (-map (lambda (x)
                                 (funcall x))
                               (butlast (lfeac--get-symbol-functions)))
                         (-map (lambda (x)
                                 (intern (format "%s:" x)))
                               (funcall 'lfeac--data-loaded-modules)))))))

(defun lfeac-ac-module-functions (m a)
  "Module M functions."
  (-map (lambda (f) (format "%s:%s" m f)) (lfeac--module-functions m a)))

;;; -------------- cache --------------

;;; define global variable for loaded modules and functions
(defvar lfeac-global-loaded-modules (list nil))
(defvar lfeac-global-loaded-module-functions (make-hash-table :test 'equal :weakness nil))

(defun lfeac-reset-module-cache ()
  "Resets the cache for symbols and modules."
  (interactive)
  (setq lfeac-global-loaded-modules (list nil))
  (message "Module cache reset."))

(defun lfeac-reset-function-cache-of-module ()
  "Resets the cache for module functions for the given module."
  (interactive)
  (let ((module (read-string "Enter module name: ")))
    (lfeac--reset-function-cache-of-module module)))

(defun lfeac--reset-function-cache-of-module (module)
  (when (and module (length> module 0))
    (if (gethash module lfeac-global-loaded-module-functions)
        (progn
          (message "Reseting functions cache for module: %s" module)
          (remhash module lfeac-global-loaded-module-functions))
      (message "No such module in cache!"))))

(defun lfeac-reset-all-module-functions-cache ()
  "Resets the cache for module functions."
  (interactive)
  (clrhash lfeac-global-loaded-module-functions))

;;; --------------- sexp -------------------

(defun lfeac--sexp-at-point (buffer current-marker)
  "Find the sexp string in the provided BUFFER and MARKER."
  (if (not (eq (current-buffer) buffer))
      (with-current-buffer buffer
        (lfeac--compute-sexp-at-point current-marker))
    (lfeac--compute-sexp-at-point current-marker)))

(defun lfeac--compute-sexp-at-point (current-marker)
  (let ((opening-bracket)
        (closing-bracket))
    (ignore-errors
      (backward-up-list 1 nil t)
      (setq opening-bracket (point-marker))
      ;;(message "closing pos: %s" opening-bracket)
      ;; find closing bracket
      (forward-list)
      (setq closing-bracket (point-marker))
      ;;(message "closing pos: %s" closing-bracket)
      ;; and finally return the string containing the sexp
      (buffer-substring-no-properties (marker-position opening-bracket)
                                      (marker-position closing-bracket)))
    ;; return to the original position
    (goto-char (marker-position current-marker))))

;;; ----------------------------------------------------------------------------

(defun lfeac--get-symbol-functions ()
  "Get the list of functions that return function symbols."
  '(lfeac--data-core-forms
    lfeac--data-basic-macro-forms
    lfeac--data-common-lisp-inspired-macros
    lfeac--data-older-scheme-inspired-macros
    lfeac--data-module-definition
    lfeac--data-standard-operators
    lfeac--data-predefined-lfe-functions
    lfeac--data-supplemental-common-lisp-functions
    lfeac--data-common-lisp-predicates
    lfeac--data-loaded-modules))

(defun lfeac--data-loaded-modules ()
  "List of loaded modules."
  (-map 'intern (lfeac--get-or-load-modules)))

(defun lfeac--module-functions (m f)
  "Get a list of functions exported from the module M that start with F."
  ;; all module functions if F is an empty string
  (-sort 'string<
         (-distinct
          (-filter (lambda (x)
                     (lfeac--string/starts-with (symbol-name x) f))
                   (-map (lambda (x) (car x))
                         (cadadr
                          (read (lfeac--sanitise-module-info
                                 (lfeac--get-or-load-module-functions m)))))))))

(defun lfeac--get-or-load-module-functions (module)
  (let ((functions (gethash module lfeac-global-loaded-module-functions)))
    (if functions
        functions
      (let ((loaded-functions (lfeac--load-module-functions module)))
        (puthash module loaded-functions lfeac-global-loaded-module-functions)
        loaded-functions))))

(defun lfeac--load-module-functions (module)
  (let ((result (lfeac--execute-on-backend
                 (format "lfe -e \"(pp (%s:module_info))\"" module))))
    (if (string-search "exception" result)
        "()"
      result)))

(defun lfeac--get-or-load-modules ()
  "Get loaded module names."
  (unless (car lfeac-global-loaded-modules)
    (setq lfeac-global-loaded-modules
          (lfeac--prep-loaded-modules
           (lfeac--load-modules))))
  (-map 'car lfeac-global-loaded-modules))

(defun lfeac--prep-loaded-modules (modules)
  (-map 'lfeac-split-string-on-spaces
        (cdr (butlast
              (lfeac--string-to-lines modules)))))
  
(defun lfeac--load-modules ()
  (lfeac--execute-on-backend "lfe -e \"(m)\" "))

(defun lfeac--execute-on-backend (cmd)
  (shell-command-to-string cmd))

(defun lfeac--sanitise-module-info (str)
  "Sanitise the string STR for reading."
  ;; now lfeac helpme does not trip over some lfe specific constructs
  ;; TODO: Elisp read might trip over other constructs, I need to find them
  (replace-regexp-in-string "#("        ;vector
                            " ("
                            (replace-regexp-in-string "#[.BM](" ;binary string or map
                                                      "  ("
                                                      str)))

(defun lfeac--string-to-lines (str)
  "Split STR into a list of lines."
  (-reject 'null
           (split-string str (format "%c" 10))))

(defun lfeac--string/starts-with (s begins)
  "Return non-nil if string S start with BEGINS."
  (cond ((>= (length s) (length begins))
         (string-equal (substring s 0 (length begins)) begins))
        (t nil)))

(defun lfeac-split-string-on-spaces (str)
  "Split STR on spaces."
  (-filter (lambda (x) (> (length x) 0))
           (split-string str " ")))

;;; ----------------------------------------------------------------------------

(defun lfeac--data-core-forms ()
  "Core forms."
  '(quote cons car cdr list tuple binary map map-get map-set map-update lambda
          match-lambda let let-function letrec-function let-macro progn if case receive
          catch try case catch when after funcall call define-module extend-module
          define-function define-macro type-test guard-bif
          include-lib))

(defun lfeac--data-basic-macro-forms ()
  "Basic macro forms."
  ;; except (: mod fun) and (mod:fun)
  ;; ? and ++
  '(list* let* flet flet* fletrec cond andalso orelse fun fun lc list-comp
      bc binary-comp match-spec))

(defun lfeac--data-common-lisp-inspired-macros ()
  "Common Lisp inspired macros."
  '(defun defmacro defsyntax macrolet syntaxlet prog1 prog2 defmodule defrecord))

(defun lfeac--data-older-scheme-inspired-macros ()
  "Older scheme inspired macros."
  '(define define-syntax let-syntax begin define-record))

(defun lfeac--data-module-definition ()
  "Symbols used in module definition."
  '(defmodule export import))

(defun lfeac--data-standard-operators ()
  "Standard operators."
  '(+ - * / > >= < =< == /= =:= =/=))

(defun lfeac--data-predefined-lfe-functions ()
  "Predefined LFE functions."
  '(acons pairlis assoc assoc-if assoc-if-not rassoc rassoc-if rassoc-if-not
          subst subst-if subst-if-not sublis macroexpand-1 macroexpand
          macroexpand-all eval))

(defun lfeac--data-supplemental-common-lisp-functions ()
  "Macros that closely mirror Hyperspec."
  ;; excluding make-lfe-bool, make-cl-bool and putf
  '(mapcar maplist mapc mapl symbol-plist symbol-name get get getl putprop
           remprop getf getf remf get-properties elt length reverse some every
           notany notevery reduce reduce reduce reduce remove remove-if
           remove-if-not remove-duplicates find find-if find-if-not
           find-duplicates position position-if position-if-not
           position-duplicates count count-if count-if-not count-duplicates car
           first cdr rest nth nthcdr last butlast subst subst-if subst-if-not
           sublis member member-if member-if-not adjoin union intersection
           set-difference set-exclusive-or subsetp acons pairlis pairlis assoc
           assoc-if assoc-if-not rassoc rassoc-if rassoc-if-not type-of coerce))

(defun lfeac--data-common-lisp-predicates ()
  "Predefied predicates, equivalent of Erlang is_* function."
  '(alivep atomp binaryp bitstringp boolp booleanp builtinp floatp funcp
           functionp intp integerp listp mapp numberp pidp process-alive-p
           recordp recordp refp referencep tuplep))

(provide 'lfe-completion)
;;; lfe-completion.el ends here

;; ---------------------------------------------
;; tests ;; more tests
;; ---------------------------------------------

(defvar-local lfeac--run-tests nil)
(defvar-local lfeac--test-buf-name "*lfeac-test*")

(eval-when-compile
  (setq lfeac--run-tests t))

(defun make-buffer (text position)
  "TEXT is the text to be inserted to the test buffer.
POSITION is where to place the curser in the buffer
where we expect autocompletion."
  (if (get-buffer lfeac--test-buf-name)
      (kill-buffer lfeac--test-buf-name))
  (let ((buf (get-buffer-create lfeac--test-buf-name)))
    (with-current-buffer buf
      (insert text)
      (goto-char position))
    buf))

(defmacro lfeac--with-fixture (buf-text buf-pos compl-text test-fun &rest body)
  `(progn
     (message "%s - Start %s" (current-time-string) ,test-fun)
     (let* ((buffer (make-buffer ,buf-text ,buf-pos))
            (marker (set-marker (make-marker) 3 buffer))
            (result (lfeac-new-autocompletions ,compl-text
                                               buffer
                                               marker)))
       ,@body
       (message "%s - End %s" (current-time-string) ,test-fun))))

(defun lfeac--test-complete-symbol ()
  (lfeac--with-fixture
   "(de)" 3 "de" "complete-symbol"
   (cl-assert
    (cl-equalp
     '("define-module" "define-function" "define-macro" "defun" "defmacro" "defsyntax"
       "defmodule" "defrecord" "define" "define-syntax" "define-record" "defmodule")
     result))))

(defun lfeac--test-complete-module--load ()
  (setq lfeac-global-loaded-modules (list nil))
  (lfeac--with-fixture
   "(fi)" 3 "fi" "complete-module--load"
   (cl-assert
    (cl-equalp
     '("file:" "file_io_server:" "file_server:" "filename:")
     (-filter (lambda (m)
                (string-suffix-p ":" m))
              result)))))

(defun lfeac--test-complete-module--cached ()
  (setq lfeac-global-loaded-modules '(("file") ("file_io_server") ("file_server") ("filename")))
  (lfeac--with-fixture
   "(fi)" 3 "fi" "complete-module--cached"
   (cl-assert
    (cl-equalp
     '("file:" "file_io_server:" "file_server:" "filename:")
     (-filter (lambda (m)
                (string-suffix-p ":" m))
              result)))))

(defun lfeac--test-complete-function-modules--load ()
  (clrhash lfeac-global-loaded-module-functions)
  (lfeac--with-fixture
   "(io:)" 4 "io:" "complete-function-modules--load"
   (cl-assert
    (cl-equalp
     (mapcar 'symbol-name
             '(io:columns io:format io:fread io:fwrite io:get_chars io:get_line
                          io:get_password io:getopts io:module_info io:nl
                          io:parse_erl_exprs io:parse_erl_form io:printable_range
                          io:put_chars io:read io:request io:requests io:rows
                          io:scan_erl_exprs io:scan_erl_form io:setopts io:write))
     result))))

(defun lfeac--test-complete-function-modules--load--subset ()
  (clrhash lfeac-global-loaded-module-functions)  
  (lfeac--with-fixture
   "(io:f)" 5 "io:f" "complete-function-modules--load--subset"
   (cl-assert
    (cl-equalp
     (mapcar 'symbol-name
             '(io:format io:fread io:fwrite))
     result))))

(defun lfeac--test-complete-function-modules--cached ()
  (puthash "io" "(#(module io)
 #(exports
   (#(nl 1)
    #(columns 0)
    #(columns 1)
    #(rows 0)
    #(rows 1))))" lfeac-global-loaded-module-functions)
  (lfeac--with-fixture
   "(io:)" 4 "io:" "complete-function-modules--cached"
   (cl-assert
    (cl-equalp
     (mapcar 'symbol-name
             '(io:columns io:nl io:rows))
     result))))

(defun lfeac--test-reset-module-cache ()
  (setq lfeac-global-loaded-modules '(a b c))
  (cl-assert (not (null lfeac-global-loaded-modules)))
  (lfeac-reset-module-cache)
  (cl-assert (not (null lfeac-global-loaded-modules)))
  (cl-assert (null (car lfeac-global-loaded-modules))))

(defun lfeac--test-reset-module-functions-cache ()
  (puthash "io" "foo" lfeac-global-loaded-module-functions)
  (lfeac--reset-function-cache-of-module "io")
  (cl-assert (null (gethash "io" lfeac-global-loaded-module-functions))))

(defun lfeac--test-reset-all-module-funtions-cache ()
  (puthash "io" "foo" lfeac-global-loaded-module-functions)
  (lfeac-reset-all-module-functions-cache)
  (cl-assert (= 0 (hash-table-count lfeac-global-loaded-module-functions))))

(when lfeac--run-tests
  (setq lfeac-global-loaded-modules (list nil))
  (clrhash lfeac-global-loaded-module-functions)
  (setq debug-on-error t)
  (lfeac--test-complete-symbol)
  (lfeac--test-complete-module--load)
  (lfeac--test-complete-module--cached)
  (lfeac--test-complete-function-modules--load)
  (lfeac--test-complete-function-modules--load--subset)
  (lfeac--test-complete-function-modules--cached)
  (lfeac--test-reset-module-cache)
  (lfeac--test-reset-module-functions-cache)
  (lfeac--test-reset-all-module-funtions-cache)
  ;; cleanup
  (setq lfeac-global-loaded-modules (list nil))
  (clrhash lfeac-global-loaded-module-functions)
  (setq debug-on-error nil))
