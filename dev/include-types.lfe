;; File for testing expansion of type declarations.

(extend-module ((export-type (o1 1))) ())

;; Some normal attributes.

(extend-module () ((dummy foo)
                   (dummystr "abc")
                   (dummyier (foo))
                   (dummiest foo bar baz)))

;; Some records.

(defrecord r1 (a 'hello) b)
(defrecord r2 (a 'hello (atom)) (b 'undefined (record r1)) c)

;; Defining types.
;; Bad type defs are in comments.

(deftype (t1) (list (integer)))

(deftype (integer x) (tuple 'not-integer (atom) x))

(defopaque (o1 x) (tuple 'not-atom (integer) (integer x)))

(deftype (t3 x y)
  (tuple 'not-atom (bert:integer) (bert:integer x) (bert:integer x y)))

(deftype (t4) (UNION (atom) (pid) 42))

(deftype (t5) (record r1))

(deftype (t6) (record r1 (a (integer)) (b (list))))

(deftype (t7) (lambda ((atom) (atom)) (tuple 't7 (integer))))

(deftype (t8) (lambda any 't8))         ;Any arity

(deftype (t9) (map))                    ;Empty map

(deftype (t10) (map ((integer) (atom)) ((atom) (pid))))

(deftype (t11) (unicode:unicode_binary))

(extend-module ((type ((t11a x) (unicode:unicode_binary x)))) ())

(deftype (t12 x) (tuple x _))

;; Binary type declarations.

(deftype t13 (binary))

(deftype t14 (bitstring))

(deftype t15 (bitstring 0 0))

(deftype t16 (bitstring 256 0))

(deftype t17 (bitstring 0 256))

(deftype t18 (bitstring 42 84))

;; Range type declarations.

(deftype t20 (range 1 42))

;; Bad type defs.
;; (deftype (t10) 'ok)                     ;Redefining t10
;; (deftype (integer) (tuple 42))          ;Redefining predefined type
;; (deftype (it1 x)                        ;Singlton type var y
;;  (tuple 'not-atom (integer) x y))
;; (deftype (it2) (sune))                  ;Unknown type (sune 0)
;; (deftype (it3 a 1) (list a))            ;Bad parameter list
;; (deftype (it4 a b) x)                   ;Singleton type vars a, b, x
