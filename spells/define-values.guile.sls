;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2026 Gwen Weinholt
;; SPDX-License-Identifier: BSD-3-Clause
#!r6rs

;; Workaround for a (probable) psyntax bug in Guile 3.0.11. There is a
;; define-values that ships with Guile, but that's broken somehow.

(library (spells define-values)
  (export define-values)
  (import (rnrs))

  (define-syntax define-values
    (lambda (form)
      (syntax-case form ()
        [(_ () exp exp* ...)
         #'(define ignored (begin exp exp* ... (if #f #f)))]

        [(_ (id ...) exp exp* ...)
         (with-syntax (((index ...)
                        (let loop [(ids #'(id ...))
                                   (i 0)]
                          (syntax-case ids ()
                            [() '()]
                            [(_ . rest) (cons i (loop #'rest (+ i 1)))]))))
           #'(begin
               (define aggregate
                 (call-with-values (lambda () exp exp* ...) vector))
               (define id (vector-ref aggregate index)) ...))]))))
