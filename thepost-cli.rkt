#!/usr/bin/env racket
#lang racket
; thepost.rkt
; simple cli nntp client
; tips taken from: http://download.racket-lang.org/docs/5.0/html/net/nntp.html
;
; Copyright (C) 2013 Lehi Toskin
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
(require net/nntp)
(require racket/include)
(include "config.rkt")

; print out the header/body contents in a sane manner
(define printer
  (lambda (lst)
    (if (null? lst)
      (display "Empty list!\n")
      (cond ((null? (rest lst)) (first lst))
	    (else (printf "~a\n" (first lst)) (printer (rest lst)))))))

; shitty bounds checking
(define bounds?
  (lambda (start end input)
    (cond [(number? input)
	   (if (or (< input start) (> input end))
	     #f
	     #t)]
	  [(string? input)
	   (if (or (string=? input start) (string=? input end))
	     #t
	     #f)]
	  [(symbol? input)
	   (if (or (eq? input start) (eq? input end))
	     #t
	     #f)]
	  (else #f))))

(define nntp-client
  (lambda ()
    (display "Rudimentary NNTP Client!\n")
    ;(display "Sending port: ")
    ;(define sender 119) ;(read))
    ;(display "Input port: ")
    ;(define receiver 119) ;(read))
    (display "Server to connect to: ")
    (define server (symbol->string (read)))
    (display "Newsgroup to read: ")
    (define newsgroup (symbol->string (read)))
    (printf "\n\nsender: ~a\nreceiver: ~s\nserver: ~a\nnewsgroup: ~a\n"
	    sender receiver server newsgroup)
    (let ((communicator (connect-to-server server #|port-number|#)))
      (define-values (total start finish) (open-news-group communicator newsgroup))
      ; begin main program loop here!
      (let loop ()
	(printf "\nWhat message would you like to read?\nIndex of ~a from ~a to ~a: "
		total start finish)
	(let [(index (read))]
	  [if (string=? (symbol->string index) "quit") ; bounds?
	    ((display "Good bye!\n") (disconnect-from-server communicator) (exit))
	    ((display "Choices: header | body | quit: ")
	     (let [(choice (read))]
	       (if (string=? (symbol->string choice) "quit") ; bounds?
		 [(display "Good bye!\n")
		  (disconnect-from-server communicator)
		  (exit)]
		 (cond [(string=? (symbol->string choice) "header")
			(let ((headers (head-of-message communicator index)))
			  (newline)
			  (printer headers))]
		       [(string=? (symbol->string choice) "body")
			(let ((bodies (body-of-message communicator index)))
			  (newline)
			  (printer bodies))]
		       (else (printf "I don't understand ~a" (symbol->string choice)))))))])
	(loop)))))

(nntp-client)
