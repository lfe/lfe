;;; Predicates

(defmacro alivep (x)
  `(is_alive))

(defmacro atomp (x)
  `(is_atom ,x))

(defmacro binaryp (x)
  `(is_binary ,x))

(defmacro bitstringp (x)
  `(is_bitstring ,x))

(defmacro boolp (x)
  `(is_boolean ,x))

(defmacro booleanp (x)
  `(is_boolean ,x))

(defmacro builtinp (mod func arity)
  `(erlang:is_builtin ,mod ,func ,arity))

(defmacro floatp (x)
  `(is_float ,x))

(defmacro funcp (x)
  `(is_function ,x))

(defmacro funcp (x arity)
  `(is_function ,x ,arity))

(defmacro functionp (x)
  `(is_function ,x))

(defmacro functionp (x arity)
  `(is_function ,x ,arity))

(defmacro intp (x)
  `(is_integer ,x))

(defmacro integerp (x)
  `(is_integer ,x))

(defmacro listp (x)
  `(is_list ,x))

(defmacro mapp (x)
  `(is_map ,x))

(defmacro numberp (x)
  `(is_number ,x))

(defmacro pidp (x)
  `(is_pid ,x))

(defmacro portp (x)
  `(is_port ,x))

(defmacro process-alive-p (pid)
  `(is_process_alive ,pid))

(defmacro recordp (x rec-tag)
  `(is_record ,x ,rec-tag))

(defmacro recordp (x rec-tag size)
  `(is_record ,x ,rec-tag ,size))

(defmacro refp (x)
  `(is_reference ,x))

(defmacro referencep (x)
  `(is_reference ,x))

(defmacro tuplep (x)
  `(is_tuple ,x))

(defmacro vectorp (x)
  `(is_tuple ,x))

(defun consp
  ((`(,a . ,b)) (when (not (is_list b)))
   'true)
  ((_) 'false))

;;; Constructors

(defmacro vector args
  `(tuple ,@args))

;;; List macros

(defmacro dolist body
  (let ((var (caar body))
        (items (cadar body))
        (body (cdr body)))
   `(lists:foreach
      (lambda (,var) (progn ,@body))
      ,items)))
