;;  This file is part of the arc package
;;  Copyright (C) 2002, 2003, 2009 by Gregor Klinke
;;
;;  This library is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Lesser General Public License as published
;;  by the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This library is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU Lesser General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Extracted from SLIB.  Original copyright was:
;;
;; "logical.scm", bit access and operations for integers for Scheme
;; Copyright (C) 1991, 1993 Aubrey Jaffer.
;;
;; Permission to copy this software, to redistribute it, and to use it
;; for any purpose is granted, subject to the following restrictions and
;; understandings.
;;
;; 1.  Any copy made of this software must include this copyright notice in
;; full.
;;
;; 2.  I have made no warrantee or representation that the operation of
;; this software will be error-free, and I am under no obligation to
;; provide any services, by way of maintenance, update, or otherwise.
;;
;; 3.  In conjunction with products arising from the use of this material,
;; there shall be no use of my name in any advertising, promotional, or
;; sales literature without prior written consent in each case.


(define (arc:logand n1 n2)
  (cond ((= n1 n2) n1)
        ((zero? n1) 0)
        ((zero? n2) 0)
        (else
         (+ (* (arc:logand (arc:ash-4 n1) (arc:ash-4 n2)) 16)
            (vector-ref (vector-ref arc:boole-and (modulo n1 16))
                        (modulo n2 16))))))

(define (arc:logior n1 n2)
  (cond ((= n1 n2) n1)
        ((zero? n1) n2)
        ((zero? n2) n1)
        (else
         (+ (* (arc:logior (arc:ash-4 n1) (arc:ash-4 n2)) 16)
            (- 15 (vector-ref (vector-ref arc:boole-and
                                          (- 15 (modulo n1 16)))
                              (- 15 (modulo n2 16))))))))

(define (arc:logxor n1 n2)
  (cond ((= n1 n2) 0)
        ((zero? n1) n2)
        ((zero? n2) n1)
        (else
         (+ (* (arc:logxor (arc:ash-4 n1) (arc:ash-4 n2)) 16)
            (vector-ref (vector-ref arc:boole-xor (modulo n1 16))
                        (modulo n2 16))))))

(define (arc:lognot n) (- -1 n))

(define (arc:ash-4 x)
  (if (negative? x)
      (+ -1 (quotient (+ 1 x) 16))
      (quotient x 16)))

(define arc:boole-xor
 '#(#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
    #(1 0 3 2 5 4 7 6 9 8 11 10 13 12 15 14)
    #(2 3 0 1 6 7 4 5 10 11 8 9 14 15 12 13)
    #(3 2 1 0 7 6 5 4 11 10 9 8 15 14 13 12)
    #(4 5 6 7 0 1 2 3 12 13 14 15 8 9 10 11)
    #(5 4 7 6 1 0 3 2 13 12 15 14 9 8 11 10)
    #(6 7 4 5 2 3 0 1 14 15 12 13 10 11 8 9)
    #(7 6 5 4 3 2 1 0 15 14 13 12 11 10 9 8)
    #(8 9 10 11 12 13 14 15 0 1 2 3 4 5 6 7)
    #(9 8 11 10 13 12 15 14 1 0 3 2 5 4 7 6)
    #(10 11 8 9 14 15 12 13 2 3 0 1 6 7 4 5)
    #(11 10 9 8 15 14 13 12 3 2 1 0 7 6 5 4)
    #(12 13 14 15 8 9 10 11 4 5 6 7 0 1 2 3)
    #(13 12 15 14 9 8 11 10 5 4 7 6 1 0 3 2)
    #(14 15 12 13 10 11 8 9 6 7 4 5 2 3 0 1)
    #(15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)))

(define arc:boole-and
 '#(#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    #(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1)
    #(0 0 2 2 0 0 2 2 0 0 2 2 0 0 2 2)
    #(0 1 2 3 0 1 2 3 0 1 2 3 0 1 2 3)
    #(0 0 0 0 4 4 4 4 0 0 0 0 4 4 4 4)
    #(0 1 0 1 4 5 4 5 0 1 0 1 4 5 4 5)
    #(0 0 2 2 4 4 6 6 0 0 2 2 4 4 6 6)
    #(0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7)
    #(0 0 0 0 0 0 0 0 8 8 8 8 8 8 8 8)
    #(0 1 0 1 0 1 0 1 8 9 8 9 8 9 8 9)
    #(0 0 2 2 0 0 2 2 8 8 10 10 8 8 10 10)
    #(0 1 2 3 0 1 2 3 8 9 10 11 8 9 10 11)
    #(0 0 0 0 4 4 4 4 8 8 8 8 12 12 12 12)
    #(0 1 0 1 4 5 4 5 8 9 8 9 12 13 12 13)
    #(0 0 2 2 4 4 6 6 8 8 10 10 12 12 14 14)
    #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
