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
;; Extracted from SLIB and adapted to arc.  Original copyright was:
;;
;; "hashtab.scm", hash tables for Scheme.
;; Copyright (c) 1992, 1993 Aubrey Jaffer
;;
;; Permission to copy this software, to redistribute it, and to use it
;; for any purpose is granted, subject to the following restrictions and
;; understandings.
;;
;; 1.  Any copy made of this software must include this copyright notice
;; in full.
;;
;; 2.  I have made no warrantee or representation that the operation of
;; this software will be error-free, and I am under no obligation to
;; provide any services, by way of maintenance, update, or otherwise.
;;
;; 3.  In conjunction with products arising from the use of this
;; material, there shall be no use of my name in any advertising,
;; promotional, or sales literature without prior written consent in
;; each case.

(define (hash:hash-char-ci char n)
  (modulo (char->integer (char-downcase char)) n))

(define hash:hash-char hash:hash-char-ci)

(define (hash:hash-symbol sym n)
  (hash:hash-string (symbol->string sym) n))

;;; This can overflow on implemenatations where inexacts have a larger
;;; range than exact integers.
(define hash:hash-number
;  (if (provided? 'inexact)
;      (lambda (num n)
;	(if (integer? num)
;	    (modulo (if (exact? num) num (inexact->exact num)) n)
;	    (hash:hash-string-ci
;	     (number->string (if (exact? num) (exact->inexact num) num))
;	     n)))
      (lambda (num n)
	(if (integer? num)
	    (modulo num n)
	    (hash:hash-string-ci (number->string num) n))));)

(define (hash:hash-string-ci str n)
  (let ((len (string-length str)))
    (if (> len 5)
	(let loop ((h (modulo 264 n)) (i 5))
	  (if (positive? i)
	      (loop (modulo (+ (* h 256)
			       (char->integer
				(char-downcase
				 (string-ref str (modulo h len)))))
			    n)
		    (- i 1))
	      h))
	(let loop ((h 0) (i (- len 1)))
	  (if (>= i 0)
	      (loop (modulo (+ (* h 256)
			       (char->integer
				(char-downcase (string-ref str i))))
			    n)
		    (- i 1))
	      h)))))

(define hash:hash-string hash:hash-string-ci)

(define (hash:hash obj n)
  (let hs ((d 10) (obj obj))
    (cond
     ((number? obj)      (hash:hash-number obj n))
     ((char? obj)        (modulo (char->integer (char-downcase obj)) n))
     ((symbol? obj)      (hash:hash-symbol obj n))
     ((string? obj)      (hash:hash-string obj n))
     ((vector? obj)
      (let ((len (vector-length obj)))
	(if (> len 5)
	    (let lp ((h 1) (i (quotient d 2)))
	      (if (positive? i)
		  (lp (modulo (+ (* h 256)
				 (hs 2 (vector-ref obj (modulo h len))))
			      n)
		      (- i 1))
		  h))
	    (let loop ((h (- n 1)) (i (- len 1)))
	      (if (>= i 0)
		  (loop (modulo (+ (* h 256) (hs (quotient d len)
						 (vector-ref obj i)))
				n)
			(- i 1))
		  h)))))
     ((pair? obj)
      (if (positive? d) (modulo (+ (hs (quotient d 2) (car obj))
				   (hs (quotient d 2) (cdr obj)))
				n)
	  1))
     (else
      (modulo
       (cond
	((null? obj)        256)
	((boolean? obj)     (if obj 257 258))
	((eof-object? obj)  259)
	((input-port? obj)  260)
	((output-port? obj) 261)
	((procedure? obj)   262)
;	((and (provided? 'RECORD) (record? obj))
;	 (let* ((rtd (record-type-descriptor obj))
;		(fns (record-type-field-names rtd))
;		(len (length fns)))
;	   (if (> len 5)
;	       (let lp ((h (modulo 266 n)) (i (quotient d 2)))
;		 (if (positive? i)
;		     (lp (modulo
;			  (+ (* h 256)
;			     (hs 2 ((record-accessor
;				     rtd (list-ref fns (modulo h len)))
;				    obj)))
;			  n)
;			 (- i 1))
;		     h))
;	       (let loop ((h (- n 1)) (i (- len 1)))
;		 (if (>= i 0)
;		     (loop (modulo
;			    (+ (* h 256)
;			       (hs (quotient d len)
;				   ((record-accessor
;				     rtd (list-ref fns (modulo h len)))
;				    obj)))
;			    n)
;			   (- i 1))
;		     h)))))
	(else               263))
       n)))))

(define arc:hash hash:hash)

(define arc:hashv hash:hash)

;;; Object-hash is somewhat expensive on copying GC systems (like
;;; PC-Scheme and MITScheme).  We use it only on strings, pairs,
;;; vectors, and records.  This also allows us to use it for both
;;; hashq and hashv.

;(if (provided? 'object-hash)
;    (set! hashv
;	  (if (provided? 'record)
;	      (lambda (obj k)
;		(if (or (string? obj) (pair? obj) (vector? obj) (record? obj))
;		    (modulo (object-hash obj) k)
;		    (hash:hash obj k)))
;	      (lambda (obj k)
;		(if (or (string? obj) (pair? obj) (vector? obj))
;		    (modulo (object-hash obj) k)
;		    (hash:hash obj k))))))

(define arc:hashq hashv)


(define (predicate->asso pred)
  (cond ((eq? eq? pred) assq)
	((eq? = pred) assv)
	((eq? eqv? pred) assv)
	((eq? char=? pred) assv)
	((eq? equal? pred) assoc)
	((eq? string=? pred) assoc)
	(else (lambda (key alist)
		(let l ((al alist))
		  (cond ((null? al) #f)
			((pred key (caar al)) (car al))
			(else (l (cdr al)))))))))

(define (alist-inquirer pred)
  (let ((assofun (predicate->asso pred)))
    (lambda (alist key)
      (let ((pair (assofun key alist)))
	(and pair (cdr pair))))))

(define (alist-associator pred)
  (let ((assofun (predicate->asso pred)))
    (lambda (alist key val)
      (let* ((pair (assofun key alist)))
	(cond (pair (set-cdr! pair val)
		    alist)
	      (else (cons (cons key val) alist)))))))

(define (alist-remover pred)
  (lambda (alist key)
    (cond ((null? alist) alist)
	  ((pred key (caar alist)) (cdr alist))
	  ((null? (cdr alist)) alist)
	  ((pred key (caadr alist))
	   (set-cdr! alist (cddr alist)) alist)
	  (else
	   (let l ((al (cdr alist)))
	     (cond ((null? (cdr al)) alist)
		   ((pred key (caadr al))
		    (set-cdr! al (cddr al)) alist)
		   (else (l (cdr al)))))))))

(define (alist-map proc alist)
  (map (lambda (pair) (cons (car pair) (proc (car pair) (cdr pair))))
       alist))

(define (alist-for-each proc alist)
  (for-each (lambda (pair) (proc (car pair) (cdr pair))) alist))


(define (predicate->hash pred)
  (cond ((eq? pred eq?) arc:hashq)
	((eq? pred eqv?) arc:hashv)
	((eq? pred equal?) arc:hash)
	((eq? pred =) arc:hashv)
	((eq? pred char=?) arc:hashv)
	((eq? pred char-ci=?) arc:hashv)
	((eq? pred string=?) arc:hash)
	((eq? pred string-ci=?) arc:hash)
	(else (arc:log 'error "unknown predicate for hash "
                       pred))))

(define (make-hash-table k) (make-vector k '()))

(define (predicate->hash-asso pred)
  (let ((hashfun (predicate->hash pred))
	(asso (predicate->asso pred)))
    (lambda (key hashtab)
      (asso key
	    (vector-ref hashtab (hashfun key (vector-length hashtab)))))))

(define (hash-inquirer pred)
  (let ((hashfun (predicate->hash pred))
	(ainq (alist-inquirer pred)))
    (lambda (hashtab key)
      (ainq (vector-ref hashtab (hashfun key (vector-length hashtab)))
	    key))))

(define (hash-associator pred)
  (let ((hashfun (predicate->hash pred))
	(asso (alist-associator pred)))
    (lambda (hashtab key val)
      (let* ((num (hashfun key (vector-length hashtab))))
	(vector-set! hashtab num
		     (asso (vector-ref hashtab num) key val)))
      hashtab)))

(define (hash-remover pred)
  (let ((hashfun (predicate->hash pred))
	(arem (alist-remover pred)))
    (lambda (hashtab key)
      (let* ((num (hashfun key (vector-length hashtab))))
	(vector-set! hashtab num
		     (arem (vector-ref hashtab num) key)))
      hashtab)))

(define (hash-map proc ht)
  (define nht (make-vector (vector-length ht)))
  (do ((i (+ -1 (vector-length ht)) (+ -1 i)))
      ((negative? i) nht)
    (vector-set!
     nht i
     (alist-map proc (vector-ref ht i)))))

(define (hash-for-each proc ht)
  (do ((i (+ -1 (vector-length ht)) (+ -1 i)))
      ((negative? i))
    (alist-for-each proc (vector-ref ht i))))
