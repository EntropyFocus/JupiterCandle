(defpackage JupiterCandle/tests/main
  (:use :cl
        :JupiterCandle
        :rove))
(in-package :JupiterCandle/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :JupiterCandle)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
