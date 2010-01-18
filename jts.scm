;(class Test (public) 
 ; (def (main args) ((public static void) (array String))
  ;     (System.out.println "Hello Java World")))
(define (begin_compile code-list)
	 (apply concat 
		(map (lambda (a)
		       (compile a #f)) code_list)))
(define (bool? code)
  (or (true? code)
      (false? code)))

(define wqu "\"")
(define concat string-append)
(define (argp_str argp)
  (if argp
      ""
      ";\n"))

(define-macro (macro-var-val pred body)
  `(define (comp-var-val code argp)
     (cond ,@(map (lambda (pre bod)
		   `((,pre code)
		     (concat ,@bod (argp_str argp)))) pred body)
	   (else
	    #f))))

(macro-var-val (string? number?  symbol? boolean? char?)
	       ((wqu code wqu)
		((number->string code))
		((symbol->string code))
		((if code
		     "true"
		     "false"))
		("'" (string code) "'")))
(define (tagged_list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (infix? exp)
  (memq (car exp) '(+ - * / % **  ^ && ||)))

(define (infix_compile inf arg_list argp)
  (let ((infix (symbol->string inf)))
     (concat (compile (car arg_list) #t)
	     (apply concat 
	      (map (lambda (arg)
		     (concat " " infix " "
			(compile arg #t))) (cdr arg_list)))
	     (argp_str argp))))
(define (binfix? exp)
     (memq (car exp) '(< > <= == =~)))

(define (binfix_compile inf arg1 arg2 argp)
  (concat (compile arg1 #t)
     " " (symbol->string inf) " " 
     (compile arg2 argp)))

(define (jasm? exp)
     (tagged_list? exp 'rasm))
(define (set? exp)
  (tagged_list? exp '=))
(define sym2str symbol->string)
(define (run? code)
  (and (pair? code) (symbol? (car code))))
(define (run-compile code argp)
  (concat (sym2str (car code))
	  "("
	  (compile (cadr code) #t)
	  (apply concat
		  (map (lambda (arg)
			 (concat ", " (compile arg #t))) (cddr code)))
	  ")" (argp_str argp)))


(define (cond? code)
  (cond-compile (cdr code)))
(define (compile code argp)
  (let ((obj-str (comp-var-val code argp)))
    (cond (obj-str
	   obj-str)
	  ((infix? code)
	   (infix_compile (car code) (cdr code) argp)
	   )
	  ((binfix? code)
	   (binfix_compile (car code) (cadr code) (caddr code) argp)
	   )
	  ((jasm? code)
	   (cadr code))
	  ((set? code)
	   (concat (symbol->string (cadr code)) 
		   " = " 
		   (compile (caddr code) #f)))
	  ((run? code)
	   (if (null? (cdr code))
	       (concat (sym2str (car code)) "()" 
		       (argp_str argp))
	       (run-compile code argp)))
	  )))
	


(print (compile '(test (add 10 30) (min 20)) #f))
