#!r6rs
(library (spells filesys compat)
  (export file-exists?
          create-directory
          delete-file
          rename-file
          
          file-regular?
          file-directory?
          file-symbolic-link?
          file-readable?
          file-writable?
          file-executable?
          file-modification-time
          file-size-in-bytes

          directory-fold*

          working-directory
          with-working-directory

          copy-file)
  (import (rnrs base)
          (rnrs lists)
          (rnrs conditions)
          (spells receive)
          (spells pathname)
          (spells time-lib)
    
          (prefix (only (mzscheme)
                        delete-directory
                        file-exists?
                        directory-exists?
                        link-exists?
                        make-directory
                        delete-file
                        rename-file-or-directory
                        file-or-directory-permissions
                        file-or-directory-modify-seconds
                        file-size
                        directory-list
                        path->string
                        copy-file
                        current-directory
                        
                        car cdr)
                  mz:))

(define x->f x->namestring)

(define (file-exists? pathname)
  (let ((f (x->f pathname)))
    (or (mz:file-exists? f) (mz:directory-exists? f)
        (mz:link-exists? f))))

(define (create-directory pathname)
  (mz:make-directory (x->f pathname)))

(define (delete-file pathname)
  (if (file-exists? pathname)
      (if (file-directory? pathname)
          (mz:delete-directory (x->f pathname))
          (mz:delete-file (x->f pathname)))))

(define (rename-file source-pathname target-pathname)
  (mz:rename-file-or-directory (x->f source-pathname) (x->f target-pathname)))

(define (file-regular? pathname)
  (mz:file-exists? (x->f pathname)))

(define (file-symbolic-link? pathname)
  (mz:link-exists? (x->f pathname)))

(define (file-directory? pathname)
  (mz:directory-exists? (x->f pathname)))

(define (file-test-p f p)
  (and (file-exists? f)
       (if (memq p (mz:file-or-directory-permissions f)) #t #f)))

(define (file-readable? pathname)
  (file-test-p (x->f pathname) 'read))
(define (file-writable? pathname)
  (file-test-p (x->f pathname) 'write))
(define (file-executable? pathname)
  (file-test-p (x->f pathname) 'execute))

(define (file-modification-time pathname)
  (posix-timestamp->time-utc
   (mz:file-or-directory-modify-seconds (x->f pathname))))

(define (file-size-in-bytes pathname)
  (mz:file-size (x->f pathname)))

;; Test wheter a filename is . or ..
(define (dot-or-dotdot? f)
  (or (string=? "." f) (string=? ".." f)))

(define (directory-fold* pathname combiner . seeds)
  (define (full-pathname entry)
    (pathname-with-file pathname (pathname-file (x->pathname entry))))
  (let loop ((entries (mz:directory-list (x->f pathname))) (seeds seeds))
    (if (null? entries)
        (apply values seeds)
        (let ((entry (mz:path->string (mz:car entries))))
          (cond ((dot-or-dotdot? entry)
                 (loop (mz:cdr entries) seeds))
                (else
                 (receive (continue? . new-seeds)
                     (apply combiner (full-pathname entry) seeds)
                   (if continue?
                       (loop (mz:cdr entries) new-seeds)
                       (apply values new-seeds)))))))))

(define (working-directory)
  (x->pathname (mz:current-directory)))

(define-syntax with-working-directory
  (syntax-rules ()
    ((with-working-directory dir body ...)
     (let ((wd (mz:current-directory)))
       (dynamic-wind
           (lambda () (mz:current-directory
                       (x->f (pathname-as-directory (x->pathname dir)))))
           (lambda () body ...)
           (lambda () (mz:current-directory wd)))))))

(define (copy-file old-file new-file)
  (mz:copy-file (x->f old-file) (x->f new-file))))
