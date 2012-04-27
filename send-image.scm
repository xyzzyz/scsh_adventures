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
