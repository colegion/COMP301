(module ps3 mzscheme
  ;Name-Surname: Ahmet Hakan Hafif (ID: 0064092)
  ;Lab Partner: Kerem Aksoy (ID: 0064234)
  ;;;;;;;;;;;;;;;;;;; PROBLEMS ;;;;;;;;;;;;;;;;;;;;
  ;; PROBLEM 1 Part A | Write your answer below here as a comment
  ; The Unary representation represent numbers as a list of #t's.
  ;It creates same number of #t with the given number which is desired to be created.
  ;It can be stored in list
  ;
  ; BigNum representation is used for representing too large numbers.
  ;It basically have different base and can be represented in a list.
  ;Numbers are calculated according to this base such as given list of (2, 0, 2) in base 4
  ;equals 4^0 * 1 + 4^1 * 0 + 4^2 * 2 = 34 in this representation.
  ;
  ;; PROBLEM 1 Part B
  ;; Unary Representation | We added a -u suffix so that both Unary and BigNum can be tested at once.

 (define (create-u n)
     (if (= n 0) '()
        (cons #t (create-u (- n 1)))))

 (define (is-zero-u? n)
    (if (null? n) #t #f))

  (define (predecessor-u n)
    (if (null? n) "error-only-positive-numbers"
     (cdr n)))

  ;; BigNum Representation | We added a -b suffix so that both Unary and BigNum can be tested at once.
  (define create-b 
    (lambda (number base)(if (= number 0) '()
                        (let ([rem (remainder number base)])
                          (cons rem (create-b (/ (- number rem) base) base))))))
  
(define (is-zero-b? lst)
  (if (null? lst) #t #f))
  
(define (predecessor-b lst base)
    (if (is-zero-b? lst)
       'error-only-positive-numbers
        (if (= (car lst) 0)
        (cons (- base 1) (predecessor-b (cdr lst) base))
        (cons (- (car lst) 1) (cdr lst))))))

  ;; PROBLEM 2 Part A
  (define count-free-occurrences
    (lambda (sym exp) (count-helper sym exp 0)))

  (define (count-helper sym exp counter)
    (if (symbol? exp)
        (if (eqv? sym exp) (+ counter 1) counter)
        (if (occurs-free? sym exp)
            (+ (count-helper sym (car exp) counter)
               (count-helper sym (cdr exp) counter)) counter)))

  
  (define occurs-free?
    (lambda (var exp)
      (cond
        ((symbol? exp) (eqv? var exp))  
        ((eqv? (car exp) 'lambda)
         (and (not (eqv? var (car (cadr exp)))) (occurs-free? var (caddr exp))))
        (else
         (or
          (occurs-free? var (car exp))
          (occurs-free? var (cadr exp)))))))

  ;; PROBLEM 2 Part B (optional)
  ((define product
    (lambda (sos1 sos2)
      (if (null? sos1)
          '()
          (append (elementPro (car sos1) sos2)
                  (product (cdr sos1) sos2)))))

  (define (elementPro a lst)
    (if (null? lst)
        '()
        (cons (list a (car lst))
              (elementPro a (cdr lst)))))
  
   ;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Don't worry about the below function, we included it to test your implemented functions and display the result in the console
  ;; As you implement your functions you can Run (the button on the top right corner) to test your implementations
  (define-syntax equal??
    (syntax-rules ()
      ((_ test-exp correct-ans)
       (let ((observed-ans test-exp))
         (if (not (equal? observed-ans correct-ans))
           (printf "Oops! ~s returned ~s, should have returned ~s~%" 'test-exp observed-ans correct-ans)
           (printf "Correct! ~s => ~s~%" 'test-exp correct-ans))))))
  
  ;;; If you don't implement the functions in order and want to test as you go, you can comment out the corresponding tests,
  ;;; otherwise, DrRacket will raise errors.
  ;; PROBLEM 1 TESTS
  ;;; For unary representation
  (display "Unary Tests\n")
  (equal?? (create-u 4) '(#t #t #t #t)) ; should return '(#t #t #t #t)
  (equal?? (is-zero-u? '(#t #t #t)) #f) ; should return #f
  (equal?? (is-zero-u? '()) #t) ; should return #t
  (equal?? (predecessor-u '(#t #t #t)) '(#t #t)) ; should return '(#t #t)
  (equal?? (predecessor-u '()) 'error-only-positive-numbers)
  (newline)

  ;;; For BigNum representation
  (display "\nBigNum Tests\n")
  (equal?? (create-b 15 4) '(3 3)) ; should return '(3 3)
  (equal?? (is-zero-b? (create-b 0 4)) #t) ; should return #t
  (equal?? (is-zero-b? (create-b 5 4)) #f) ; should return #f
  (equal?? (predecessor-b (create-b 31 4) 4) '(2 3 1)) ; should return '(2 3 1)
  (equal?? (predecessor-b (create-b 64 4) 4) '(3 3 3 0)) ; should return '(3 3 3 0)
  (equal?? (predecessor-b (create-b 0 4) 4) 'error-only-positive-numbers) ; should return error

  (newline)

  ;; PROBLEM 2 Part A TESTS
  (display "\nCount Free Occurences Tests\n")
  (equal?? (count-free-occurrences 'x 'x) 1) ;1
  (equal?? (count-free-occurrences 'x 'y) 0) ;0
  (equal?? (count-free-occurrences 'x '(lambda (x) (x y))) 0) ;0
  (equal?? (count-free-occurrences 'x '(lambda (y) (x x))) 2) ;2
  (equal?? (count-free-occurrences 'x '((lambda (xx) x) (x y))) 2) ;2
  (equal?? (count-free-occurrences 'x '((lambda (x) (y x)) (lambda (y) (x (lambda (z) x))))) 2) ;2

  ;; PROBLEM 2 Part B TESTS (Optional)
  (display "\nCartesian Product Tests\n")
  (equal?? (product '(x y) '(a b)) '((x a) (x b) (y a) (y b)))
  (equal?? (product '() '(a b)) '())
  
)