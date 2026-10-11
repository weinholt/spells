#!r6rs
;;; sysutils.chezscheme.sls --- Chez Scheme sysutils

;; Copyright (C) 2010, 2015 Andreas Rottmann <a.rottmann@gmx.at>
;; Copyright (C) 2018 Gwen Weinholt

;; Authors: Andreas Rottmann <a.rottmann@gmx.at>
;;          Gwen Weinholt

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(library (spells sysutils compat)
  (export find-exec-path
          host-info)
  (import (rnrs base)
          (rnrs lists)
          (rnrs records syntactic)
          (only (srfi :13 strings) string-prefix? string-suffix?)
          (spells string-utils)
          (spells filesys)
          (only (chezscheme) getenv directory-separator machine-type))

  (define (find-exec-path prog)
    (let ((paths (string-split (getenv "PATH")
                               (if (eqv? (directory-separator) #\/)
                                   #\: #\;))))
      (find-file prog paths file-executable?)))

  (define machine-cpus
    '(("a6" "x86_64")
      ("i3" "i386")
      ("arm32" "arm")
      ("arm64" "aarch64")
      ("ppc32" "powerpc")
      ("rv64" "riscv64")
      ("la64" "loongarch64")))

  (define machine-systems
    '(("le" "unknown" "linux-gnu")
      ("fb" "unknown" "freebsd")
      ("nb" "unknown" "netbsd")
      ("ob" "unknown" "openbsd")
      ("osx" "apple" "darwin")
      ("s2" "sun" "solaris")
      ("qnx" "pc" "nto")
      ("nt" "w64" "mingw32")))

  (define (host-info)
    (let* ((mt (symbol->string (machine-type)))
           (mt (if (string-prefix? "t" mt)   ;threaded
                   (substring mt 1 (string-length mt))
                   mt))
           (cpu (find (lambda (e) (string-prefix? (car e) mt)) machine-cpus))
           (sys (find (lambda (e) (string-suffix? (car e) mt)) machine-systems)))
      (values (if cpu (cadr cpu) "unknown")
              (if sys (cadr sys) "unknown")
              (if sys (caddr sys) "unknown")))))
