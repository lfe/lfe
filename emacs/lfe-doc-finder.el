;;; lfe-doc-finder.el --- Code for finding LFE documentation and auto-completion

;; Copyright (c) 2017 Jacek Podkanski
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

;; Author Jacek Podkanski

;;; Commentary:

;;; WARNING: I made a big mistake calling the new syntax the old syntax
;;; correct new syntax is (module:function) not (: module function)

;;; Also code needs a good refactoring.
;;; We need sensible function names, caching exported function names and other
;;; things.

;;; Usage:

;;; Load this file by adding it to the load-path and running:
;;; (require 'lfe-doc-finder)
;;; or loading it directly
;;; (load "emacs/lfe-doc-finder.el")

;;; LOOK-UP

;;; M-x lfedoc-helpme s-/
;;; Will hopefully take you to a relevant page in Erlang documentation.

;;; M-x lfedoc-inspect
;;; Will print to the mini buffer the sexp at the cursor.

;;; AUTO-COMPLETION s-1
;;; Your file has to be in lfe-mode for auto completion to work.
;;; Placing cursor in various LFE sexps and pressing s-1 will give you various
;;; auto completion options that later hopefully will be integrated with Emacs.
;;; Even not it will give you a list of modules available by default and explore
;;; their exported functions.

;;; Testing:

;;; At the bottom of the file I have tests that also explain how the code is
;;; supposed to behave in various contexts.

;;; Code:

(require 'browse-url)

(global-set-key (kbd "s-1") 'company-lfe-backend)
(global-set-key (kbd "s-7") 'lfedoc-module-functions) ; with arity
(global-set-key (kbd "s-/") 'lfedoc-helpme) ; works with complete sexps and arity
;;; ----------------------------------------------------------------------------

;; (load "/home/jacek/Programming/Pyrulis/Emacs/vendor/lfe-doc-finder.el")

(defun company-lfe-backend (command &optional arg &rest ignored)
  "Get auto completion COMMAND for ARG and IGNORED."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-lfe-backend))
    (prefix (and (or (eq major-mode 'lfe-mode)
                     (eq major-mode 'inferior-lfe-mode))
                 (company-grab-symbol)))
    (candidates
     (lfedoc-new-ac-at-point arg))))

(add-to-list 'company-backends 'company-lfe-backend)

;;; ----------------------------------------------------------------------------

;;; define global variable for loaded modules
(defvar lfedoc-global-loaded-modules (list nil))

(defun lfedoc-query-loaded-modules ()
  "Get loaded module names."
  (if (car lfedoc-global-loaded-modules)
      (-map 'car
            lfedoc-global-loaded-modules)
    ;; refresh if not loaded
    (-map 'car
          (lfedoc-refresh-loaded-modules))))

(defun lfedoc-query-module-functions (module)
  "Get Exports information about loaded MODULE."
  (let ((exports-seen))
    (-sort 'string<
           (-flatten
            (-map 'lfedoc-split-string-on-spaces
                  (cdr
                   (-reject 'null ; reject everything before the "Exports: " line
                            (-map
                             (lambda (x)
                               (when (equal "Exports: " x)
                                 (setq exports-seen t))
                               (when exports-seen
                                 x))
                             (lfedoc-string-to-lines
                              (shell-command-to-string
                               (format "lfe -e \"(m (quote %s))\""
                                       module)))))))))))

;;; You need to run it after every reload of this file.
(defun lfedoc-refresh-loaded-modules ()
  "Refresh the list of loaded modules."
  (interactive)
  (setq lfedoc-global-loaded-modules
        (-map 'lfedoc-split-string-on-spaces
              (cdr (butlast
                    (lfedoc-string-to-lines
                     (shell-command-to-string (format "lfe -e \"%s\" "
                                                      "(m)")))))))
  (princ "Modules have been refreshed.")
  lfedoc-global-loaded-modules)

;;; ----------------------------------------------------------------------------

(defun lfedoc-string-to-lines (str)
  "Split STR into a list of lines."
  (-reject 'null
           (split-string str (format "%c" 10))))

(defun lfedoc-split-string-on-spaces (str)
  "Split STR on spaces."
  (-filter (lambda (x) (> (length x) 0))
           (split-string str " ")))

;;; ----------------------------------------------------------------------------
(defun lfedoc-module-functions ()
  "Get a list of module exported functions that start with given character(s)
or all functions if no function characters are given."
  (interactive)
  (let ((call-struct (lfedoc-sexp (sexp-at-point))))
    (if (nth 0 call-struct)
        (pp
         (-filter
          (lambda (x)
            (if (nth 1 call-struct)
                ;; if any character of the function given show possible completions
                ;; otherwise show all available functions
                (lfedoc-string/starts-with
                 x
                 (format "%s"  (nth 1 call-struct)))
              t))
          (lfedoc-query-module-functions (nth 0 call-struct)))))))

(defun lfedoc-module-functions-2 (m f)
  "Get a list of functions exported from the module M that start with F."
  ;; all module functions if F is an empty string
  (-sort 'string<
         (-distinct
          (-filter (lambda (x)
                     (lfedoc-string/starts-with (symbol-name x) f))
                   (-map (lambda (x) (car x))
                         (cadadr
                          (read (lfedoc-sanitise
                                 (shell-command-to-string (format "lfe -e \"%s\" "
                                                                  (format "(pp (%s:module_info))" m)))))))))))

(defun lfedoc-modules ()
  "Get list of loaded modules that start with given character(s)."
  (interactive)
  (let ((call-struct (lfedoc-sexp (sexp-at-point))))
    (pp (if (nth 0 call-struct)
            (-filter (lambda (x)
                       (lfedoc-string/starts-with
                         x
                        (format "%s" (nth 0 call-struct))))
                     (lfedoc-query-loaded-modules))
          (lfedoc-query-loaded-modules)))))

(defun lfedoc-modules-2 (symb)
  "Get modules that start with SYMB."
  ;; all modules if symb is nil
  (if symb
      (-map 'intern
            (-filter (lambda (x)
                       (lfedoc-string/starts-with x (symbol-name symb)))
                     (lfedoc-query-loaded-modules)))
    (lfedoc-query-loaded-modules)))

(defun lfedoc-functions ()
  "Get list of known user guide functions that start with given character(s)."
  (interactive)
  ;; we get the character from the call struct
  (let ((call-struct (lfedoc-sexp (sexp-at-point))))
    (when (nth 1 call-struct)
      (princ
       (-distinct
        (-sort 'string<
               (-flatten
                (-map 'cdr
                      (lfedoc-find-symbol-autocompletions
                       (nth 1 call-struct))))))))))

(defun lfedoc-autocomplete-function ()
  "Autocomplete the function divided into user guide sections."
  (interactive)
  (let ((call-struct (lfedoc-sexp (sexp-at-point))))
    (if (nth 1 call-struct)
        (princ (list 'autocompleting 'function (nth 1 call-struct)
                     call-struct
                     (lfedoc-find-symbol-autocompletions (nth 1 call-struct)))))))

(defun lfedoc-string/starts-with (s begins)
  "Return non-nil if string S start with BEGINS."
  (cond ((>= (length s) (length begins))
         (string-equal (substring s 0 (length begins)) begins))
        (t nil)))

(defun lfedoc-get-symbol-functions ()
  "Get the list of functions that return function symbols."
  '(lfedoc-data-core-forms
    lfedoc-data-basic-macro-forms
    lfedoc-data-common-lisp-inspired-macros
    lfedoc-data-older-scheme-inspired-macros
    lfedoc-data-module-definition
    lfedoc-data-standard-operators
    lfedoc-data-predefined-lfe-functions
    lfedoc-data-supplemental-common-lisp-functions
    lfedoc-data-common-lisp-predicates
    lfedoc-data-loaded-modules))

(defun lfedoc-find-symbol-autocompletions (symb)
  "Find symbol SYMB in known symbols and return the function names that return it."
  ;; example (lfedoc-find-symbol-functions  (quote car))
  ;; when symb is nil return everything
  (-filter (lambda (x) (not (null (second x))))
           (-map (lambda (f) (list f
                                   (-filter (lambda (sf)
                                              (if symb
                                                  (lfedoc-string/starts-with
                                                   (symbol-name sf)
                                                   (symbol-name symb))
                                                t))
                                            (funcall f))))
                 (lfedoc-get-symbol-functions))))

(defun lfedoc-module-or-module-functions-autocompletions (s)
  "Get auto-completions for modules starting with symbol S, or functions of module S."
  (let ((found-modules (lfedoc-modules-2 s)))
    (if (-any (lambda (x)
                (equal x s))
              found-modules)
        (list (list 'modules found-modules)
              (list 'module-functions (lfedoc-module-functions-2 (symbol-name s)
                                                                 "")))
      (list (list
             'modules found-modules)))))

;;; ############################################################################

(defun lfedoc-new-ac-at-point (arg)
  (interactive)
  "Get ne auto completions at point."
  (let ((se (sexp-at-point)))
    (lfedoc-new-autocompletions se arg)))

(defun lfedoc-new-autocompletions (sexp-str arg)
  "New auto completion for SEXP-STR and ARG."
  (let ((ss (split-string arg ":")))
      (if (equal 1 (length ss))
          (lfedoc-ac-symbols-and-modules arg)
        (lfedoc-ac-module-functions (car ss) (cadr ss)))))

(defun lfedoc-ac-symbols-and-modules (arg)
  "Write me."

  (cl-remove-if-not
          (lambda (c) (string-prefix-p arg c))
          (-map (lambda (x) (format "%s" x))
                (-flatten (list (-map (lambda (x) (funcall x)) (butlast (lfedoc-get-symbol-functions)))
                                (-map (lambda (x) (intern (format "%s:" x))) (funcall 'lfedoc-data-loaded-modules)))))))

(defun lfedoc-ac-module-functions (m a)
  "Module M functions."
  (-map (lambda (f) (format "%s:%s" m f))  (lfedoc-module-functions-2 m a)))

;;; ############################################################################

(defun lfedoc-sexp-autocompletion-at-point ()
  "Auto complete sexp at point."
  (let ((se  (sexp-at-point)))
    ;; at the moment we print the result, in future we will pass it to future
    ;; completion UI
    (pp
     (lfedoc-sexp-autocompletion se))))

;;; autocompletion for various sexp forms
(defun lfedoc-sexp-autocompletion (sexp-str)
  "Read SEXP-STR and get autocompletion."
  (let ((rs (lfedoc-read-sexp sexp-str)))
    (let  ((split-symbol (when (car rs)
                           (split-string (symbol-name (car rs)) ":"))))
        (cond ((equal rs nil)               ; ()
               (lfedoc-find-symbol-autocompletions nil))
              ((equal rs '(:))              ; (:)
               (lfedoc-data-loaded-modules))
              ((and (equal (car rs)         ; (: m) or (: mod)
                           :)
                    (equal (length rs)
                           2))
               (lfedoc-module-or-module-functions-autocompletions (cadr rs)))
              ((and (equal (car rs)         ; (: mod f)
                           :)
                    (equal (length rs)
                           3))
               (lfedoc-module-functions-2 (symbol-name (cadr rs))
                                          (symbol-name (caddr rs))))
              ((and (equal (length split-symbol)
                           2)               ; (mod:) or (mod:f)
                    (equal (length rs)
                           1))
               (lfedoc-module-functions-2 (car split-symbol)
                                          (cadr split-symbol)))
              ((equal (length rs)           ; (a) any
                      1)
               (lfedoc-find-symbol-autocompletions (car rs)))))))

(defun lfedoc-inspect ()
  "Print sexp."
  (interactive)
  (let ((sexp-str (sexp-at-point)))
    ;; show read source and the sanitised version used for reading by Emacs
    (princ (list sexp-str
                 'sanitised-version (lfedoc-sanitise sexp-str)
                 'macroexpanded
                 (format "lfe -e \"(io:format (macroexpand-all (quote %s )))\""
                         sexp-str)))))

(defun sexp-at-point ()
  "Find the sexp string."
  (let ((original-buffer (current-buffer))
        (original-marker (point-marker))
        (opening-bracket)
        (closing-bracket))
    ;; find opening bracket
    (backward-up-list)
    (setq opening-bracket (point-marker))
    ;; find closing bracket
    (forward-list)
    (setq closing-bracket (point-marker))
    ;; return to the original position
    (goto-char (marker-position original-marker))
    ;; and finally return the string containing the sexp
    (buffer-substring-no-properties (marker-position opening-bracket)
                                    (marker-position closing-bracket))))

(defun lfedoc-helpme ()
  "Go to Erlang website for help."
  (interactive)
  ;; Read sanitised sexp and extract model and function for browse-url look-up
  (let ((call-struct (lfedoc-sexp (sexp-at-point))))
    (if (car call-struct)
        (progn
          (if (or (equalp "cl" (car call-struct)) ; different forms are read differently
                  (equalp 'cl (car call-struct)))
              ;; browse Hyperspec
              (browse-url
               (format "http://clhs.lisp.se/Body/f_%s.htm" (nth 1 call-struct)))
            ;; browse Erlang documentation
            (browse-url
             (apply 'format
                    (cons "http://erlang.org/doc/man/%s.html#%s-%d"
                          call-struct)))))
      (princ (list "search"
                   (lfedoc-find-symbol-functions (nth 1 call-struct))
                   "for"
                   (nth 1 call-struct)
                   'arity (nth 2 call-struct))))))

(defun lfedoc-read-sexp (str)
  "Convert STR to sexp."
  (read (lfedoc-sanitise str)))

(defun lfedoc-sexp (str)
  "Convert STR to sexp."
  (lfedoc-call-struct (read (lfedoc-sanitise str))))

(defun lfedoc-call-struct (my-sexp)
  "Examine MY-SEXP and return a structure representing module function and arity."
  (cond ((lfedoc-new-erlang-callp my-sexp)
         (lfedoc-new-erlang-call-args my-sexp))
        ((lfedoc-old-erlang-callp my-sexp)
         (lfedoc-old-erlang-call-args my-sexp))
        (t
         (lfedoc-unknown-code my-sexp))))

(defun lfedoc-sanitise (str)
  "Sanitise the string STR for reading."
  ;; now lfedoc helpme does not trip over some lfe specific constructs
  ;; TODO: Elisp read might trip over other constructs, I need to find them
  (replace-regexp-in-string "#("        ;vector
                            " ("
                            (replace-regexp-in-string "#[.BM](" ;binary string or map
                                                      "  ("
                                                      str)))
(defun lfedoc-cl-function-callp (sl)
  "Check if the SL is a supplemental Lisp function.")

(defun lfedoc-cl-function-call-args (sl)
  "Try to loop up for SL using the Hyperspec equivalent.")

(defun lfedoc-new-erlang-callp (sl)
  "Check id the SL is the new Erlang call syntax."
  (eql (car sl) :))

(defun lfedoc-new-erlang-call-args (sl)
  "Get new Erlang call info for the documentation look-up list SL."
  (cond ((and (nth 1 sl)
              (nth 2 sl))
         (list (nth 1 sl) (nth 2 sl) (- (length sl) 3)))
        ((nth 1 sl)
         (list (nth 1 sl) nil nil))
        (t
         (list nil nil nil))))

(defun lfedoc-old-erlang-callp (sl)
  "Check id the SL is the old Erlang call syntax."
  (eql (length (split-string (symbol-name (car sl))
                             ":"))
       2))

(defun lfedoc-old-erlang-call-args (sl)
  "Get old Erlang call info for the documentation look-up list SL."
  (let ((call-str (split-string (symbol-name (car sl)) ":")))
    (list (nth 0 call-str)
          (nth 1 call-str)
          (- (length sl) 1))))

(defun lfedoc-unknown-code (sl)
  "Provide unrecognised module information from SL."
  ;; because it's not a module:function of : module function
  ;; we returm nil as module but still return the function and arity
  (list nil (car sl) (- (length sl) 1)))

(defun lfedoc-find-symbol-functions (symb)
  "Find symbol SYMB in known symbols and return the function names that return it."
  ;; example (lfedoc-find-symbol-functions  (quote car))
  (-reject 'null
           (-map (lambda (f) (when (-contains? (funcall f) symb) f))
                 (lfedoc-get-symbol-functions))))

;;; ----------------------------------------------------------------------------

(defun lfedoc-data-core-forms ()
  "Core forms."
  '(quote cons car cdr list tuple binary map map-get map-set map-update lambda
          match-lambda let let-function letrec-function let-macro progn if case receive
          catch try case catch when after funcall call define-module extend-module
          define-function define-macro type-test guard-bif
          include-lib))

(defun lfedoc-data-basic-macro-forms ()
  "Basic macro forms."
  ;; except (: mod fun) and (mod:fun)
  ;; ? and ++
  '(list* let* flet flet* fletrec cond andalso orelse fun fun lc list-comp
      bc binary-comp match-spec))

(defun lfedoc-data-common-lisp-inspired-macros ()
  "Common Lisp inspired macros."
  '(defun defmacro defsyntax macrolet syntaxlet prog1 prog2 defmodule defrecord))

(defun lfedoc-data-older-scheme-inspired-macros ()
  "Older scheme inspired macros."
  '(define define define-syntax let-syntax begin define-record))

(defun lfedoc-data-module-definition ()
  "Symbols used in module definition."
  '(defmodule export import))

(defun lfedoc-data-standard-operators ()
  "Standard operators."
  '(+ - * / > >= < =< == /= =:= =/=))

(defun lfedoc-data-predefined-lfe-functions ()
  "Predefined LFE functions."
  '(acons pairlis assoc assoc-if assoc-if-not rassoc rassoc-if rassoc-if-not
          subst subst-if subst-if-not sublis macroexpand-1 macroexpand
          macroexpand-all eval))

(defun lfedoc-data-supplemental-common-lisp-functions ()
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

(defun lfedoc-data-common-lisp-predicates ()
  "Predefied predicates, equivalent of Erlang is_* function."
  '(alivep atomp binaryp bitstringp boolp booleanp builtinp floatp funcp
           functionp intp integerp listp mapp numberp pidp process-alive-p
           recordp recordp refp referencep tuplep))

(defun lfedoc-data-loaded-modules ()
  "List of loaded modules."
  (-map 'intern (lfedoc-query-loaded-modules)))

;; Trying another set of correct values. for which we should have working
;; auto-completion

;; using (lfedoc-find-symbol-autocompletions nil)
;; () all modules and user_guide functions in groups
;; () alternatively all modules or all functions in groups
;; (: ) all modules

;; (: m) all modules starting with m, m is the incomplete name
;; (: mod) all mod functions, mod is the full name of the module
;; if mod is a full name of the module but at the same time is a
;; prefix/incomplete name of other modules we might have show both possible
;; module name completions and module functions

;; (: mod f) all mod functions starting with f
;; (a) all modules and user_guide functions starting with a - (pp (lfedoc-find-symbol-autocompletions 'a))
;; (mod:) all mod functions
;; (mod:f) all mod functions starting with f
;; otherwise nothing

;;; in scratch buffer evaluate (lfedoc-test-all)
(defun lfedoc-test-all ()
  "Test all test cases."
  (let ((error-count 0))
    (let ((test-case (lambda (tc)
                       (if tc
                           (princ "Y")
                         (progn
                           (incf error-count)
                           (princ "E"))))))
      (lfedoc-query-loaded-modules)
      (princ (format "%ctesting%c" 10 10))
      ;; my test cases
      (funcall test-case (not (>= 1 2)))
      ;; all modules
      (funcall test-case (equal 74 (length (lfedoc-query-loaded-modules))))
      (funcall test-case (equal "application" (car (lfedoc-query-loaded-modules))))
      (funcall test-case (equal "zlib" (car (last (lfedoc-query-loaded-modules)))))
      (funcall test-case (equal 'application (car (lfedoc-data-loaded-modules))))
      (funcall test-case (equal 'zlib (car (last (lfedoc-data-loaded-modules)))))
      ;; all modules
      (funcall test-case (equal 74 (length (lfedoc-modules-2 nil))))
      ;; all modules starting with c
      (funcall test-case (equal '(c code code_server) (lfedoc-modules-2 'c)))
      ;; all functions in module io
      (funcall test-case (equal 22 (length (lfedoc-module-functions-2 "io" ""))))
      ;; all functions in module io that start with p
      (funcall test-case (equal '(parse_erl_exprs parse_erl_form printable_range put_chars)
                                (lfedoc-module-functions-2 "io" "p")))
      ;; test reading string representations of sexps and resulting lengths
      ;; note that (:), (a), (mod:) and (mod:f) all have length 1, so we will
      ;; have to check if it is a colon, contains a colon (hopefully only 1),
      ;; or is a list containing a symbol
      ;; $ lfe -e "(pp (macroexpand-all '(mo:d:fu:n)))"
      ;; $ lfe -e "(pp (macroexpand-all '(mod:fun)))"

      ;; test parsing and reading
      (funcall test-case (equal  '(("()" nil 0) ("(: )" (:) 1) ("(: a)" (: a) 2)
                                   ("(: mod)" (: mod) 2) ("(: mod f)" (: mod f) 3)
                                   ("(: mod f 1)"  (: mod f 1) 4)
                                   ("(a)" (a) 1) ("(mod:)" (mod:) 1)
                                   ("(mod:f)" (mod:f) 1) ("(mod:f 1)" (mod:f 1) 2))
                                 (-map (lambda (x) (list x
                                                         (lfedoc-read-sexp x)
                                                         (length (lfedoc-read-sexp x))
                                                         ))
                                       (list "()" "(: )" "(: a)" "(: mod)"
                                             "(: mod f)" "(: mod f 1)" "(a)"
                                             "(mod:)" "(mod:f)" "(mod:f 1)"))))
      ;; test back-end function invocation
      (funcall test-case (equal
                          (lfedoc-sexp-autocompletion "()")
                          (lfedoc-find-symbol-autocompletions nil)))
      (funcall test-case (equal
                          (lfedoc-sexp-autocompletion "(:)")
                          (lfedoc-data-loaded-modules)))
      (funcall test-case (equal
                          (lfedoc-sexp-autocompletion "(: i)")
                          (lfedoc-module-or-module-functions-autocompletions 'i)))
      (funcall test-case (equal
                          (lfedoc-sexp-autocompletion "(: io f)")
                          (lfedoc-module-functions-2 "io" "f")))
      (funcall test-case (equal
                          (lfedoc-sexp-autocompletion "(io:)")
                          (lfedoc-module-functions-2 "io" "")))
      (funcall test-case (equal
                          (lfedoc-sexp-autocompletion "(io:f)")
                          (lfedoc-module-functions-2 "io" "f")))
      (funcall test-case (equal
                          (lfedoc-sexp-autocompletion "(a)")
                          (lfedoc-find-symbol-autocompletions 'a)))
      ;; conclusion

      (princ (format "%cerror count %s%c" 10 error-count 10))
      nil)))

(provide 'lfe-doc-finder)
;;; lfe-doc-finder.el ends here
