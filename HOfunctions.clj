;----------------------------------------------------------
; Problem Set #4: Higher-Order Functions
; Date: March 31, 2023.
; Authors:
;          A01748222 Joahan Javier Garcia Fernandez
;          A0******* Benjamín Cruz
;          A0******* Luis Fernando
;----------------------------------------------------------

;Namespace
(ns HOfunctions
  (:require [clojure.test :refer [deftest is run-tests]])
  (:require [clojure.algo.generic.math-functions
             :refer [abs approx=]]))

;Exercises


;#1

;#2
(defn there-exists-one
  " Returns true if there is exactly one element in s hat satisfies pred?, otherwise returns false."
  [pred? s]
  (= 1 (count (filter pred? s))))

;#3
(defn linear-search
  "returns the index where the first occurrence of x is found in vct, or nil if not found."
  [vct x eq-fun]
  (loop [i 0]
    (cond
      (= i (count vct)) nil
      (eq-fun x (vct i)) i
      :else (recur (inc i)))))






;Test

(deftest test-there-exists-one
  (is (not (there-exists-one pos?
                             ())))
  (is (there-exists-one pos?
                        '(-1 -10 4 -5 -2 -1)))
  (is (there-exists-one neg?
                        '(-1)))
  (is (not (there-exists-one symbol?
                             '(4 8 15 16 23 42))))
  (is (there-exists-one symbol?
                        '(4 8 15 sixteen 23 42))))

(deftest test-linear-search
  (is (nil? (linear-search [] 5 =)))
  (is (= 0 (linear-search [5] 5 =)))
  (is (= 4 (linear-search
             [48 77 30 31 5 20 91 92
              69 97 28 32 17 18 96]
             5
             =)))
  (is (= 3 (linear-search
             ["red" "blue" "green" "black" "white"]
             "black"
             identical?)))
  (is (nil? (linear-search
              [48 77 30 31 5 20 91 92
               69 97 28 32 17 18 96]
              96.0
              =)))
  (is (= 14 (linear-search
              [48 77 30 31 5 20 91 92
               69 97 28 32 17 18 96]
              96.0
              ==)))
  (is (= 8 (linear-search
             [48 77 30 31 5 20 91 92
              69 97 28 32 17 18 96]
             70
             #(<= (abs (- %1 %2)) 1)))))

;Run tests
(run-tests)
