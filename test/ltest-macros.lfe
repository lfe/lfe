;; Include EUnit macros
(include-lib "eunit/include/eunit.hrl")

;;;===================================================================
;;; Helper functions
;;;===================================================================

(eval-when-compile
  (defun to-unders (atm)
    (re:replace (atom_to_list atm) "-" "_" '(#(return list) global))))


;;;===================================================================
;;; Test definition macros
;;;===================================================================

(defmacro deftest
  "Define a standard EUnit test."
  ((cons name body)
   (let ((name_test (list_to_atom (++ (to-unders name) "_test"))))
     `(progn (defun ,name_test () ,@body)
             (extend-module () ((export (,name_test 0))))))))


;;;===================================================================
;;; Assertion macros
;;;===================================================================

(defmacro is (bool-expression)
  "Assert `bool-expression` evaluates to `'true`."
  `(assert ,bool-expression))

(defmacro is-not (bool-expression)
  "Assert `bool-expression` evaluates to `'false`."
  `(assertNot ,bool-expression))

(defmacro is-match (guard expression)
  "Assert `guard` matches `expression`.

  The main reason to use [[is-match/2]], instead of matching with `=`,
  is that it produces more detailed error messages."
  `(assertMatch ,guard ,expression))

(defmacro is-not-match (guard expression)
  "The inverse case of [[is-match/2]], for convenience."
  `(assertNotMatch ,guard ,expression))

(defmacro is-equal (value expression)
  "Assert `expression` evaluates to `value`."
  `(assertEqual ,value ,expression))

(defmacro is-error
  "Equivalent to [[is-exception/3]] with `'error` as `expected-class`."
  (`(,expression) `(is-error _ ,expression))
  (`(,error ,body) `(assertError ,error ,body)))

(defmacro is-not-error
  "The inverse case of `is-error/{1,2}`, for convenience."
  (`(,expression)
   `(is-not-error _ ,expression))
  (`(,expected-term ,expression)
   `(is-not-exception 'error ,expected-term ,expression)))

(defmacro is-not-exception
  "The inverse case of [[is-exception/3]], for convenience."
  (`(,expression)
   `(is-not-exception _ _ ,expression))
  (`(,expected-class ,expected-term ,expression)
   `(assertNotException ,expected-class ,expected-term ,expression)))


;;;===================================================================
;;; Clojure-inspired macros
;;;===================================================================

;; TODO: pull these functions/macros out

;; Based on clojure.walk
(eval-when-compile
  ;; FIXME: walk more data structures
  (defun walk
    ([inner outer form] (when (is_list form))
     (funcall outer (lists:map inner form)))
    ([_inner outer form] (funcall outer form)))
  (defun postwalk (f form)
    ;; N.B. Due to implementation details, we can't use
    ;;          (clj:partial #'postwalk/2 f)
    (walk (lambda (inner-form) (postwalk f inner-form)) f form))
  (defun postwalk-replace (proplist form)
    (postwalk (lambda (|-X-|) (proplists:get_value |-X-| proplist |-X-|)) form))
  (defun apply-template (arglist expr values)
    (orelse (is_list arglist) (error 'badarg (list arglist expr values)))
    (orelse (lists:all #'is_atom/1 arglist))
    (postwalk-replace (lists:zip arglist values) expr))
  ;; Based on #'clojure.template/do-template
  (defmacro do-template
    (`(,arglist ,expr . ,values)
     (let ((|-LEN-| (length arglist)))
       `(list ,@(lists:map (lambda (|-A-|) (apply-template arglist expr |-A-|))
                  (clj:partition |-LEN-| values)))))))

;; Based on #'clojure.test/are
(defmacro are*
  (`(() ,expr) `(is* 'true))
  (`(,arglist ,expr . ,args)
   `(do-template ,arglist (is ,expr) ,@args))
  (_ (error 'badarg (list* arglist expr args))))
