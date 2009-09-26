;;; tests.scm --- Test directory

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:


((systems spells)
 (files
  ("lazy.scm"
   (spells lazy)
   (rnrs io ports))
  ("record-types.scm"
   (spells record-types)
   (srfi :8 receive))
  ("algebraic-types.scm"
   (spells algebraic-types))
  ("pathname.scm" (spells pathname) (spells misc) (rnrs lists) (rnrs sorting))
  ("filesys.scm"
   (spells filesys) (spells pathname)
   (except (srfi :1 lists) for-each map)
   (rnrs control) (rnrs exceptions))
  ("string-utils.scm" (spells string-utils))
  ("format.scm" (spells format))
  ("misc.scm" (spells misc) (rnrs lists))
  ("match.scm" (spells match))
  ;; these are temporarily disabled; need to port to trc-testing
  #;("opt-args.scm" (spells opt-args))
  #;("table.scm" (spells table))
  ("delimited-readers.scm"
   (spells delimited-readers)
   (spells misc)
   (srfi :8 receive)
   (srfi :13 strings)
   (rnrs io ports))
  ("operations.scm" (spells operations) (rnrs lists))
  ("ports.scm" (spells ports) (srfi :8 receive) (rnrs io ports))
  ("logging.scm" (spells logging) (rnrs lists))
  ("process.scm" (spells process) (srfi :8 receive) (srfi :13 strings) (rnrs io ports))
  ("sysutils.scm"
   (spells sysutils)
   (only (srfi :1 lists) count)
   (rnrs lists))
  ("foreign.scm" (spells foreign) (spells misc) (spells filesys))
  ("foof-loop.scm"
   (spells foof-loop)
   (rnrs io simple)
   (rnrs io ports)
   (rnrs mutable-strings)
   (rnrs r5rs))
  ("awk.scm"
   (spells awk)
   (spells ascii)
   (spells delimited-readers)
   (rnrs io ports)
   (rnrs lists)
   (only (srfi :1) make-list iota)
   (only (srfi :13) string-join))
  ("delimited-control.scm"
   (spells delimited-control))
  ("zipper-tree.scm"
   (spells zipper-tree))
  ("xvector.scm"
   (spells xvector)
   (rnrs control))
  ("fmt.scm" 'no-default-imports
   (spells fmt)
   (rnrs)
   (rnrs r5rs)
   (rnrs mutable-pairs)
   (srfi :0 cond-expand)
   (srfi :26 cut)
   (srfi :64 testing)
   (spells include)
   (spells misc)
   (spells string-utils))
))
