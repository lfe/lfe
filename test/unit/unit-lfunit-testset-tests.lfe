(defmodule unit-lfunit-testset-tests
  (export all)
  (import
    (from lfunit
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))))

(include-lib "include/lfunit.lfe")

(deftest testset-with-one-element
  (list (is 'true)))

(deftest testset-with-two-elements
  (list (is 'true)
        (is-not 'false)))

(deftest testset-with-three-elements
  (list (is 'true)
        (is-not 'false)
        (is-equal 2 2)))

(deftest testset-nested
  (list (is 'true)
        (is-not 'false)
        (is-equal 2 2)
        (list (is 'true)
              (is-not 'false)
              (is-equal 1 1))))

(deftest testset-deeply-nested
  (list (is 'true)
        (is-not 'false)
        (is-equal 1 1)
        (list (is 'true)
              (is-not 'false)
              (is-equal 2 2)
              (list (is 'true)
                    (is-not 'false)
                    (is-equal 3 3)
                    (try
                      (progn
                        (is-equal 3 4)
                        (error 'unexpected-test-succes))
                      (catch
                        ((tuple type value _)
                         (check-failed-assert value 'assertEqual_failed))))))))

(deftest two-testsets
  (list (is 'true)
        (is-not 'false))
  (list (is-equal 1 1)
        (is-not-equal 1 2)))

(deftest three-testsets
  (list (is 'true)
        (is-not 'false))
  (list (is-equal 1 1)
        (is-not-equal 1 2))
  (list (is-equal 42 (* 2 (+ 1 2 3 4 5 6)))))
