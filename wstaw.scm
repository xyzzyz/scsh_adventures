#!/usr/bin/scsh \
-o srfi-37 -e main -s
!#


(define (usage)
  (format (error-output-port)
          "Usage: ~A [-p|--private] [-t tags|--tags=tags] [-c comment|--comment=comment] file...~%"
          (argv 0)))

(define (ensure-curl)
  (or (zero? (run (which curl) (> /dev/null) (= 2 1)))
      (error "No curl found.")))

(define *wstaw-url* "http://wstaw.org/")

(define (send-image image-path private? comment tags)
  (let ((image-arg `(-F ,(format #f "pic=@~A" image-path)))
        (private-arg (if private? '(-F "is_private=yes") '()))
        (comment-arg (if comment `(-F ,(format #f "comment=~A" comment)) '()))
        (tags-arg (if tags `(-F ,(format #f "tags=~A" tags)) '())))
    (receive (port child) (run/port+proc (curl ,*wstaw-url*
                                               ,@private-arg
                                               ,@comment-arg
                                               ,@tags-arg
                                               ,@image-arg
                                               -L -s -S))
             (let* ((output (port->string port))
                    (status (wait child)))
               (if (zero? status)
                   (extract-url output)
                   (error "Wstaw: curl error."))))))

(define (extract-url source)
  (let ((match (regexp-search
                (rx (: "value=\""
                       (submatch (: "http://wstaw.org/m/"
                                    (* (~ "\""))))
                       "\""))
                source)))
    (if (not match)
        (error "Could not extract link.")
        (format #f "~A" (match:substring match 1)))))

(define (parse-command-line)
  (let* ((private? #f)
         (comment #f)
         (tags #f)
         (images #f))
    (set! images
          (args-fold command-line-arguments
                     (list
                      (option '(#\h "help") #f #f
                              (lambda (opt name args images)
                                (usage)
                                (exit 0)))
                      (option '(#\p "private") #f #f
                              (lambda (opt name args images)
                                (set! private? #t)
                                images))
                      (option '(#\t "tags") #t #f
                              (lambda (opt name args images)
                                (set! tags args)
                                images))
                      (option '(#\c "comment") #t #f
                              (lambda (opt name args images)
                                (set! comment args)
                                images)))
                     (lambda (opt name arg images)
                       (format (error-output-port) "Unrecognized option: ~A~%" name)
                       (usage)
                       (exit -1))
                     (lambda (image images)
                       (cons image images))
                     '()))
    (values images private? comment tags)))

(define (main arg)
  (ensure-curl)
  (receive (images private? comment tags) (parse-command-line)
           (if (null? images)
               (begin (usage) (exit -1))
               (map (lambda (file)
                      (format #t "~A~%" (send-image file private? comment tags)))
                    images))))
