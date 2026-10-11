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
  (import (except (rnrs) define-record-type)
	  (srfi :9)
	  (srfi :106)
	  (spells network utils))

  (define-record-type connection
    (%make-connection socket input-port output-port)
    connection?
    (socket connection-socket)
    (input-port connection-input-port)
    (output-port connection-output-port))

  (define (make-connection socket)
    (%make-connection socket
                      (socket-input-port socket)
                      (socket-output-port socket)))

  (define-record-type listener
    (make-listener socket)
    listener?
    (socket listener-socket))

  (define (close-connection conn)
    (close-port (connection-output-port conn))
    (close-port (connection-input-port conn))
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
