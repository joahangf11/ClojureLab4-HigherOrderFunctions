;----------------------------------------------------------
; Problem Set #4: Higher-Order Functions
; Date: April 12, 2023.
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
(defn argswap
  " takes as input a two argument function f and returns a new function that behaves like f but with the order of its two arguments swapped"
  [f]
  (fn [x y] (f y x)))
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

;#4
(defn deriv
  "Takes f and h as its input which represents the derivative of f given a certain value of h"
  [f h]
  (fn [x] (/ (- (f(+ x h)) (f x)) h)))

;5
(defn newton
  "takes f and n as its inputs, and returns the corresponding value of x"
  [f n]
  (if (zero? n)
    0
    (let [x-n-1 (newton f (dec n))]
      (- x-n-1
         (/ (f x-n-1)
            ((deriv f 0.0001) x-n-1))))))

;6
(defn integral
  "that takes as inputs a, b, n, and f. It returns the value of the integral, using Simpson’s rule."
  [a b n f]
  (let [h (/ (- b a) n)
        v1 (f a)
        v3 (f b) ; Lo mismo que evaluarlo al final (+ a (* n h))
        sip (loop [acc 0
                   i 0]
              (if (> i (/ (- n 2) 2))
                acc
                (recur (+ acc (f (+ a (* (+ (* 2 i) 1) h)))) (inc i))))
        sp (loop [acc 0
                  i 1]
             (if (> i (/ (- n 2) 2))
               acc
               (recur (+ acc (f (+ a (* (* 2 i) h)))) (inc i))))]
    (*
      (/ h 3)
      (+
        v1 v3 (* 4 sip) (* 2 sp)))))

;7


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

(defn f [x] (* x x x))
(def df (deriv f 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))

(deftest test-deriv
  (is (approx= 75 (df 5) 0.05))
  (is (approx= 30 (ddf 5) 0.05))
  (is (approx= 6 (dddf 5) 0.05)))


(deftest test-newton
  (is (approx= 10.0
               (newton (fn [x] (- x 10))
                       1)
               0.00001))
  (is (approx= -0.5
               (newton (fn [x] (+ (* 4 x) 2))
                       1)
               0.00001))
  (is (approx= -1.0
               (newton (fn [x] (+ (* x x x) 1))
                       50)
               0.00001))
  (is (approx= -1.02987
               (newton (fn [x] (+ (Math/cos x)
                                  (* 0.5 x)))
                       5)
               0.00001)))

(deftest test-integral
  (is (= 1/4 (integral 0 1 10 (fn [x] (* x x x)))))
  (is (= 21/4
         (integral 1 2 10
                   (fn [x]
                     (integral 3 4 10
                               (fn [y]
                                 (* x y))))))))



;Run tests
(run-tests)
