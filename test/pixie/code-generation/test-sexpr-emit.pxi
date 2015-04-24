(ns pixie.code-generation.test-sexpr-emit
  (:require [pixie.code-generation.analyzer :refer [analyze]]
            [pixie.code-generation.sexpr-emit :refer [to-sexpr]]
            [pixie.test :refer :all]))


(deftest test-to-sexpr
  (assert-table [x y] (assert= x (to-sexpr (analyze y)))
                '(do 1) '(do 1)
                '(do 1 2 3) '(do 1 2 3)
                '(if 1 2 3) '(if 1 2 3)
                '(if 1 2 nil) '(if 1 2)
                '(let* [x 1] (do x)) '(let [x 1] x)
                '(let* [x 1 y 2] (do y)) '(let [x 1 y 2] y)
                '(fn* foo ([x] (do x))) '(fn foo [x] x)))

