;(define-module jts
 ; (export tmacro map-quote concat sym2str compile))
;(select-module jts)
;(class Test (public) 
 ; (def (main args) ((public static void) |String[]|)
  ;     (System.out.println "Hello Java World")))
(define (begin-compile code-list)
	 (apply concat 
		(map (lambda (a)
		       (compile a #f)) code-list)))

(define macro-list '())

(define (define-macro? exp)
  (tagged-list? exp 'macro))

(define (macro? exp)
     (if (pair? exp) 
	 (memq (car exp) macro-list)
       #f))

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
(define (tagged-list? exp tag)
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
     (tagged-list? exp 'jasm))
(define (set? exp)
  (tagged-list? exp '=))
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

(define (val-mac? code)
  (tagged-list? code 'val-mac))

(define (def-macro-compile macode)
  (eval (cons 'define macode) (interaction-environment)))
(define (cond? code)
  (tagged-list? code 'cond))
(define (cond-compile code)
  (concat "if(" (compile (caar code) #t) ") {\n"
	  (begin-compile (cdar code))
          "}"
	  (if (null? (cdr code))
	      "\n"
	      (apply concat
		     (map (lambda (con)
			    (if (eq? 'else (car con))
				(concat " else {\n"
					(begin-compile (cdr con)) "}\n")
				(concat "else if(" (compile (car con) #t)
					"){\n" (begin-compile (cdr con)) 
					"}")))
			  (cdr code))))
	  "\n"))
(define (macro-args-compile code)
  (let args2quote ((lis (cdr code)) (ret '()))
    (if (null? lis)
	(cons (car code) (reverse ret))
	(args2quote (cdr lis) (cons (list 'quote (car lis))
				    ret)))))
(define (macro-compile code)
;  (eval code (interaction-environment)))
  (if (null? (cdr code))
      (eval code (interaction-environment))
      (eval (macro-args-compile code) (interaction-environment))))

(define (begin? code)
  (or (tagged-list? code 'begin)
      (tagged-list? code 'do)))
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
	  ((define-macro? code)
	   (set! macro-list (cons (caadr code) macro-list))
	   (def-macro-compile (cdr code))
	   ""
	  )
	  ((macro? code)
	   (let ((run-code (macro-compile code)))
	      ;run-code))
	     (compile run-code argp)))
	  ((val-mac? code)
	   (compile (cadr code) #t))
	  ((begin? code)
	   (begin-compile (cdr code)))
	  ((jasm? code)
	   (cadr code))
	  ((set? code)
	   (concat (symbol->string (cadr code)) 
		   " = " 
		   (compile (caddr code) #t) (argp_str argp)))
	  ((cond? code)
	   (cond-compile (cdr code)))
	  ((run? code)
	   (if (null? (cdr code))
	       (concat (sym2str (car code)) "()" 
		       (argp_str argp))
	       (run-compile code argp)))
	  )))
	

(define macro-test '((macro (add x)
		       `(= ,x (+ ,x 1)))
		     (add b)))

(define (s-read file-name)
  (with-input-from-file file-name
    (lambda ()
      (let loop ((ls1 '()) (s (read)))
	(if (eof-object? s)
	    (reverse ls1)
	    (loop (cons s ls1) (read)))))))

(define out-p '())
(define (program-print program-list out)
  (map (lambda (code)
	 (display 
	  (compile code #f) out)) program-list))
(define (map-sym-concat lis)
  (apply concat 
	 (map (lambda (arg)
		(concat (sym2str arg) " ")) lis)))
(compile '(macro (class name sh-lis  extend . body)
	   `(begin
	      (jasm ,(apply concat 
			    (map (lambda (arg)
				   (concat (sym2str arg) " ")) sh-lis)))
	      (jasm "class ")
	      (val-mac ,name)
	      (jasm " extends ")
	      (jasm ,(map-sym-concat extend))
	      (jasm "{\n")
	      ,@body
	      (jasm "}\n"))) #t)

(compile '(macro (def name-args kata . body)
	    `(begin
	       (jasm ,(apply concat
			     (map (lambda (arg)
				    (concat (sym2str arg) " ")) (car kata))))
	       (val-mac ,(car name-args))
	       (jasm "(")
	       (jasm ,(if (or (null? kata) (null? (cdr kata)))
			  ""
			  (concat 
			   (sym2str (cadr kata)) " " 
			   (sym2str (cadr name-args)))))
	       (jasm ,(if (or (null? kata) (null? (cdr kata)) (null? (cddr kata)))
			  ""
			  (apply 
			   concat
			   (map (lambda (k arg)
				  (concat ", " (sym2str k) " " (sym2str arg)))
				(cddr kata) (cddr name-args)))))
	       (jasm ") {\n")
	       ,@body
	       (jasm "}\n"))) #f)
(define (main args)
  (print args)
  (let ((program-list (s-read (cadr args))))
    (if (null? (cddr args))
	(set! out-p (open-output-file "out"))
	(set! out-p (open-output-file (caddr args))))
    (program-print program-list out-p)))
  
(define-macro (tmacro name args . body)
  (set! macro-list (cons name macro-list))
  (let ((mac-name (gensym)))
    `(begin
       (define ,mac-name (lambda ,args ,@body))
       (define-macro (,name ,@args)
	 ,(list 'quasiquote
		(list 'compile (cons mac-name (dot-list->list args '())) #f))))))

(define (map-quote lis)
    (map (lambda (val)
	   `(quote ,val)) lis))

(define (dot-list->list lis result)
  (cond ((null? lis)
	 (reverse result))
	((not (pair? lis))
	 (reverse (cons (list 'unquote-splicing (list 'map-quote lis)) result)))
	(else
	 (dot-list->list (cdr lis) 
			 (cons (list 'quote (list 'unquote (car lis))) result)))))
	 
	 

;(provide "./jts")
