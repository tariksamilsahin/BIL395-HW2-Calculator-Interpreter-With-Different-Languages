(use-modules (ice-9 readline))
(use-modules (ice-9 rdelim))
(use-modules (srfi srfi-1)) ; for fold

;; --- Tokenizer ---

(define (tokenize input)
  (let loop ((chars (string->list input)) (current "") (tokens '()))
    (cond
     ((null? chars)
      (if (string=? current "")
          (reverse tokens)
          (reverse (cons current tokens))))
     ((char-whitespace? (car chars))
      (if (string=? current "")
          (loop (cdr chars) "" tokens)
          (loop (cdr chars) "" (cons current tokens))))
     ((member (car chars) '(#\+ #\- #\* #\/ #\( #\)))
      (let ((op (string (car chars))))
        (if (string=? current "")
            (loop (cdr chars) "" (cons op tokens))
            (loop (cdr chars) "" (cons op (cons current tokens))))))
     (else
      (loop (cdr chars) (string-append current (string (car chars))) tokens)))))

;; --- Parser ---

(define (parse tokens)
  ;; expr → term ((+|-) term)*
  (define (parse-expr tokens)
    (call-with-values
     (lambda () (parse-term tokens))
     (lambda (lhs tokens)
       (parse-expr-rest lhs tokens))))

  (define (parse-expr-rest lhs tokens)
    (if (null? tokens)
        (values lhs '())
        (let ((op (car tokens)))
          (cond
           ((or (string=? op "+") (string=? op "-"))
            (call-with-values
             (lambda () (parse-term (cdr tokens)))
             (lambda (rhs tokens2)
               (parse-expr-rest (list (string->symbol op) lhs rhs) tokens2))))
           (else (values lhs tokens))))))

  ;; term → factor ((*|/) factor)*
  (define (parse-term tokens)
    (call-with-values
     (lambda () (parse-factor tokens))
     (lambda (lhs tokens)
       (parse-term-rest lhs tokens))))

  (define (parse-term-rest lhs tokens)
    (if (null? tokens)
        (values lhs '())
        (let ((op (car tokens)))
          (cond
           ((or (string=? op "*") (string=? op "/"))
            (call-with-values
             (lambda () (parse-factor (cdr tokens)))
             (lambda (rhs tokens2)
               (parse-term-rest (list (string->symbol op) lhs rhs) tokens2))))
           (else (values lhs tokens))))))

  ;; factor → number | ( expr )
  (define (parse-factor tokens)
    (let ((tok (car tokens)))
      (cond
       ((string=? tok "(")
        (call-with-values
         (lambda () (parse-expr (cdr tokens)))
         (lambda (node rest)
           (if (and (not (null? rest)) (string=? (car rest) ")"))
               (values node (cdr rest))
               (error "Expected ')'")))))
       ((string->number tok)
        (values (string->number tok) (cdr tokens)))
       (else
        (error (string-append "Unexpected token: " tok))))))

  (parse-expr tokens))

;; --- REPL ---

(define (start-calculator)
  (display "Scheme Calculator — type 'q' to quit.\n")
  (let loop ()
    (display ">> ")
    (let ((line (read-line)))
      (if (or (eof-object? line) (string=? line "q"))
          (display "Goodbye!\n")
          (let* ((tokens (tokenize line))
                 (expr (call-with-values (lambda () (parse tokens))
                         (lambda (e _r) e))))
            (display "= ")
            (display (eval expr (interaction-environment)))
            (newline)
            (loop))))))

(start-calculator)

