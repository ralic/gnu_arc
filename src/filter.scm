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

;; filters a input file to an output file.  In-file and out-file must not
;; be equal.  Words surrounded by @ are recognized as keys and looked up in
;; a alist.  The alist entries cadr is printed out, when such a key is
;; found.  An empty keyword (@@) is printed a single @ to the output
;; stream.  If a keyword is not found in the alist nothing is printed to
;; the outstream and an error message is given.

(define (arc:filter-file in-file out-file table)
  (arc:filter-file* in-file out-file table #\@))

(define (arc:filter-file* in-file out-file table sepc)
  (let* ((port (open-input-file in-file))
         (out (open-output-file out-file))
         (state 'copy-pass) 
         (key ()))
    (do ((c (read-char port) (read-char port)))
        ((eof-object? c) 'done)
      (case state
        ((copy-pass) (if (eq? c sepc)
                         (begin
                           (set! state 'parse-key)
                           (set! key ()))
                         (write-char c out)))
        ((parse-key) (if (eq? c sepc)
                         (let* ((k (list->string (reverse key)))
                                (a (assoc k table)))
                           (if (= (string-length k) 0)
                               (write-char sepc out)
                               (if a
                                   (display (cadr a) out)
                                   (arc:msg "key '" k "' not known")))
                           (set! state 'copy-pass))
                         (set! key (cons c key))))))
    (close-input-port port)
    (close-output-port out)))


;;Keep this comment at the end of the file 
;;Local variables:
;;mode: scheme
;;End:
