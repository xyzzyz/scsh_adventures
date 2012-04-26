#!/usr/bin/scsh \
-e main -s
!#


(define (usage)
  (error (format #f "Usage: ~A [-p] [-t \"tag1 tag2 ...\"] [-c \"comment...\"] file" (argv 0))))

(define (ensure-curl)
  (or (zero? (run (which curl) (> /dev/null) (= 2 1)))
      (error "No curl found.")))

(define *wstaw-url* "http://wstaw.org/")

(define (send-image image-path private? comment tags)
  (let ((image-arg `(-F ,(format #f "pic=@~A" image-path)))
        (private-arg (if private? 
                         '(-F "is_private=yes")
                         '()))
        (comment-arg (if comment 
                         `(-F ,(format #f "comment=~A" comment))
                         '()))
        (tags-arg (if tags 
                      `(-F ,(format #f "tags=~A" tags))
                      '())))
    (extract-url (run/string (curl ,*wstaw-url*
                                   ,@private-arg 
                                   ,@comment-arg
                                   ,@tags-arg
                                   ,@image-arg
                                   -L -s -S)))))

(define (extract-url source)
  (let ((match (regexp-search 
                (rx (: "value=\"" 
                       (submatch (: "http://wstaw.org/m/" 
                                    (* (~ "\"")))) 
                       "\"")) 
                source)))
    (if (not match)
        (error "Could not extract link.")
        (format #t "~A~%" (match:substring match 1)))))

(define (parse-command-line args)
  (if (or (null? args)
          (string= "-h" (car args)))
      (usage))
  (let* ((private? #f)
         (comment #f)
         (tags #f)
         (filename #f))
    (letrec ((arg? 
              (lambda (a)
                (any (lambda (item) 
                       (string= item a))
                     '("-t" "-p" "-c"))))
             (parse 
              (lambda (args)
                (cond ((null? args) #t)
                      ((string= "-p" (car args)) 
                       (begin (set! private? #t)
                              (parse (cdr args))))
                      ((and (string= "-t" (car args))
                            (not (null? (cdr args)))
                            (not (arg? (cadr args))))
                       (begin (set! tags (cadr args))
                              (parse (cddr args))))
                      ((and (string= "-c" (car args))
                            (not (null? (cdr args)))
                            (not (arg? (cadr args))))
                       (begin (set! comment (cadr args))
                              (parse (cddr args))))
                      ((not filename)
                       (begin (set! filename (car args))
                              (parse (cdr args))))
                      (#t (usage))))))
      (begin (parse args)
             (if filename
                 (values filename private? comment tags)
                 (usage))))))

(define (main arg)
  (ensure-curl)
  (receive (file private? comment tags) (parse-command-line 
                                         (cdr (command-line)))
           (send-image file private? comment tags)))
