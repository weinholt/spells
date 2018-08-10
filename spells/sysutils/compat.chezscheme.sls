#!r6rs
;;; sysutils.chezscheme.sls --- Chez Scheme sysutils

;; Copyright (C) 2010, 2015 Andreas Rottmann <a.rottmann@gmx.at>
;; Copyright (C) 2018 Göran Weinholt <goran@weinholt.se>

;; Authors: Andreas Rottmann <a.rottmann@gmx.at>
;;          Göran Weinholt <goran@weinholt.se>

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

  (define (host-info)
    (let ((mt (symbol->string (machine-type))))
      (let-values
          (((cpu)
            (cond ((or (string-prefix? "a6" mt) (string-prefix? "ta6" mt)) "x86_64")
                  ((or (string-prefix? "i3" mt) (string-prefix? "ti3" mt)) "i386")
                  ((or (string-prefix? "arm32" mt) (string-prefix? "tarm32" mt)) "arm")
                  ((or (string-prefix? "ppc32" mt) (string-prefix? "tppc32" mt)) "powerpc")
                  (else "unknown")))
           ((vendor os)
            (cond ((string-suffix? "le" mt) (values "unknown" "linux-gnu"))
                  ((string-suffix? "fb" mt) (values "unknown" "freebsd"))
                  ((string-suffix? "nb" mt) (values "unknown" "netbsd"))
                  ((string-suffix? "ob" mt) (values "unknown" "openbsd"))
                  ((string-suffix? "osx" mt) (values "apple" "darwin"))
                  ((string-suffix? "s2" mt) (values "sun" "solaris"))
                  ((string-suffix? "qnx" mt) (values "pc" "nto")) ;neutrino
                  ((string-suffix? "nt" mt) (values "pc" "cygwin"))
                  (else (values "unknown" "unknown")))))
        (values cpu vendor os)))))
