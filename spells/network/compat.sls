#!r6rs
; compat.sls - (spells network) compatibility layer using srfi 106

;; Copyright (C) 2020 Martin Becze <mjbecze@gmail.com>

;; Author: Martin Becze <mjbecze@gmail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:
;; This has been tested with Loko.
;;; Code:

(library (spells network compat)
  (export connection?
	  connection-input-port
	  connection-output-port
	  close-connection
	  listener?
	  listener-accept
	  listener-address
	  close-listener
	  open-tcp-connection
	  open-tcp-listener)
  (import (scheme base)
	  (srfi :9)
	  (srfi :106)
	  (spells network utils))

  (define-record-type connection
    (make-connection socket)
    connection?
    (socket connection-socket))

  (define-record-type listener
    (make-listener socket)
    listener?
    (socket listener-socket))

  (define (connection-input-port conn)
    (socket-input-port (connection-socket conn)))

  (define (connection-output-port conn)
    (socket-output-port (connection-socket conn)))

  (define (close-connection conn)
    (socket-close (connection-socket conn)))

  (define (listener-accept listener)
    (make-connection (socket-accept (listener-socket listener))))

  (define (close-listener listener)
    (socket-close (listener-socket listener)))

  (define (listener-address listener) #f)

  (define (open-tcp-connection address service)
    (make-connection (make-client-socket address
					 (cond ((integer? service)
						(number->string service))
					       ((symbol? service)
						(symbol->string service))
					       (else
						service)))))

  (define (open-tcp-listener . maybe-options)
    (let-options* (if (null? maybe-options)
		      '()
		      (car maybe-options))
		  ((service #f))
		  (unless service
		    (raise-impl-restriction 'open-tcp-listener
					    "ephemeral ports not supported"))
		  (make-listener
		   (make-server-socket (number->string service))))))
