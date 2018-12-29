;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


"you need to write the triangle method"
(defun flatten-1-level-down (l)
  (loop for a in l appending a))

(defun remove-one (what from-what)
  (remove what from-what :count 1))

(defun unique (a-list)
  (remove-duplicates a-list :test #'equalp))

(defun unordered-pairs (a-list) 
  (mapcon (lambda (tail) (mapcar (lambda (x) (cons (car tail) x)) (cdr tail))) a-list))

(defun orders-of-three (a-list-of-3) 
  (unique (flatten-1-level-down (mapcar 
    (lambda (_1st) 
      (mapcar 
	(lambda (_2nd) 
	  (cons
	    _1st
	  (cons 
	    _2nd
	    (remove-one _2nd (remove-one _1st a-list-of-3))))) ;; '(_3rd)
	(remove-one _1st a-list-of-3))) 
    a-list-of-3))))

(defun triangle-ineq-p (a b c) 
  (> (+ a b) c))

(defun fails-to-be-a-triangle (edges) 
  (some (lambda (b) (not (or b nil))) 
	(mapcar (lambda (x) (apply #'triangle-ineq-p x)) (orders-of-three edges))))

(define-condition triangle-error  (error) ())

(defun triangle (a b c)
  (if (fails-to-be-a-triangle (list a b c))
    (error 'triangle-error))
  (let* (
	(pairs (unordered-pairs (list a b c)))
	(eq-for-pairs (mapcar (lambda (pair-b) (= (car pair-b) (cdr pair-b))) pairs)))
    (cond
      ((every #'identity eq-for-pairs) :equilateral) ;; hack for 'every element is true'
      ((some #'identity eq-for-pairs) :isosceles)
      (t :scalene))))



(define-test test-equilateral-triangles-have-equal-sides
    (assert-equal :equilateral (triangle 2 2 2))
    (assert-equal :equilateral (triangle 10 10 10)))


(define-test test-isosceles-triangles-have-two-equal-sides
    (assert-equal :isosceles (triangle 3 4 4))
    (assert-equal :isosceles (triangle 4 3 4))
    (assert-equal :isosceles (triangle 4 4 3))
    (assert-equal :isosceles (triangle 10 10 2)))


(define-test test-scalene-triangles-have-no-equal-sides
    (assert-equal :scalene (triangle 3 4 5))
    (assert-equal :scalene (triangle 10 11 12))
    (assert-equal :scalene (triangle 5 4 2)))


(define-test test-illegal-triangles-throw-exceptions
    (assert-error 'triangle-error (triangle 0 0 0))
    (assert-error 'triangle-error (triangle 3 4 -5))
    (assert-error 'triangle-error (triangle 1 1 3))
    (assert-error 'triangle-error (triangle 2 4 2)))
