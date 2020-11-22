#!r6rs
;;; sysutils.loko.sls --- Loko sysutils

;; Copyright (C) 2010, 2015 Andreas Rottmann <a.rottmann@gmx.at>
;; Copyright (C) 2020 Göran Weinholt

;; Author: Göran Weinholt

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
          (srfi :8 receive)
          (spells string-utils)
          (spells filesys)
          (srfi :98)
          (only (loko) machine-type))

  (define (find-exec-path prog)
    (let ((paths (string-split (get-environment-variable "PATH") #\:)))
      (find-file prog paths file-executable?)))

  (define (host-info)
    (let ((mt (machine-type)))
      (values (symbol->string (vector-ref mt 0))
              "unknown"
              (symbol->string (vector-ref mt 1))))) )
