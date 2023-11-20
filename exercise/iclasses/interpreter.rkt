#lang racket

#|
Rômulo Luiz Araujo Souza Soares
201665219C
|#

(require dcc019/util/env
         dcc019/util/memory
         dcc019/exercise/iclasses/ast)

(provide value-of-program)

; value-of :: Exp -> ExpVal
(define (value-of exp Δ)
  (match exp
    [(ast:int v) v]
    [(ast:bool v) v]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:not e) (not (value-of e Δ))]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    [(ast:var v) (deref(apply-env Δ v))]
    [(ast:let (ast:var x) e1 e2) (value-of e2 (extend-env x (value-of e1 Δ) Δ))]
    [(ast:send e (ast:var mth) args) (make-send e mth args Δ)]
    [(ast:super (ast:var c) args) (make-super c args Δ)]
    [(ast:self) (make-self Δ)]
    [(ast:new (ast:var c) args) (make-new c args Δ)]
    [e (raise-user-error "unimplemented-construction: " e)]
    ))

; result-of :: Stmt -> Env -> State -> State
(define (result-of stmt Δ)
  (match stmt
   [(ast:assign (ast:var x) e) (begin (setref! (apply-env Δ x) (value-of e Δ)) 42)]
    [(ast:print e) (display (value-of e Δ))]
    [(ast:return e) (value-of e Δ)]
    [(ast:block stmts) (for ([s stmts]) (result-of s Δ))]
    [(ast:if-stmt e s1 s2) ((if (value-of e Δ) (result-of s1 Δ) (result-of s2 Δ)))]
    [(ast:while e s) (if (value-of e Δ) (begin (result-of s Δ) (result-of stmt Δ)) 'done)]
    ;[(ast:local-decl (ast:var x) s) (result-of s (extend-env x (newref 'null) Δ))]
    [(ast:local-decl (ast:var x) s) (let ([new-ref (newref '())]) (result-of s (extend-env x new-ref Δ)))]
    [(ast:send e (ast:var mth) args) (make-send e mth args Δ)]
    [(ast:super (ast:var c) args) (make-super c args Δ)]
    [e (raise-user-error "unimplemented-construction: " e)]
    ))

; Explicação pg 336/12
(define (value-of-program prog)
  (empty-store)
  (match prog
    [(ast:prog decls stmt)
     (begin
       ; you must collect all the classes declared and building its respectively environment
       ; execute the prog expression in the correct environment
       (initialize-class-env decls)
       (result-of stmt init-env))]))


;Struct necessarias
(struct class (supe-name field-name method-env))
(struct object (class-name fields))
(struct method (vars body super-name field-name))



; self -> pg 336/12
(define (make-self env) (apply-env env "self"))

; send -> pg 337/13
(define (make-send obj-exp method-name args Δ)
  (let* ([args (values-of-exps args Δ)]
         [obj (value-of obj-exp Δ)])
    (apply-method (find-method (object-class-name obj) method-name) obj args)))

; super -> pg 337/13
(define (make-super method-name args Δ)
  (let* ([args (values-of-exps args Δ)]
        [obj (apply-env Δ "self")])
    (apply-method (find-method (apply-env Δ "super") method-name) obj args)))

; new -> pg 337/13
(define (make-new class-name args Δ)
  (let* ([args (values-of-exps args Δ)]
        [obj (new-object class-name)])
    (apply-method (find-method class-name "initialize") obj args)
    obj))

; initializa uma nova instancia de um object -> pg 340/16
(define (new-object class-name)
  (object class-name
          (map (lambda (field-name) (newref (list "uninitialized-field" field-name)))
               (class-field-name (lookup-class class-name)))))


; apply-method -> pg341/17
; Implementar depois
(define (apply-method method self args)
  (let* ([vars (method-vars method)]
         [body (method-body method)]
         [env (extend-env "self" self (extend-env "super" (method-super-name method) empty-env))]
         [env1 (extend-envs (method-field-name method) (object-fields self) env)]
         [Δ (extend-envs vars (map newref args) env1)]
         [result (result-of body Δ)])
    result))

; extend-env -> n tem no livro
(define (extend-envs vars values env)
  (if (or (null? vars) (null? values))
      env (extend-envs (cdr vars) (cdr values) (extend-env (car vars) (car values) env))))


; Variavel global do ambiente das classes -> pg 343/19
(define class-env '())


; add-to-class-env! -> pg 341/19
(define (add-to-class-env class-name class)
  (set! class-env (cons (list class-name class) class-env)))


; lookup-class
(define (lookup-class name)
  (let* ([maybe-pair (assoc name class-env)])
    (if maybe-pair (cadr maybe-pair)
        (raise-user-error "report-uknown-class" name))))


; initialize clas env -> pg 344/20
(define (initialize-class-env decls)
  (set! class-env
        (list (list "object" (class #f '() '()))))
  (for-each initialize-class-decl decls))


;initialize class decl -> pg 344/19
(define (initialize-class-decl decl)
  (match decl
    [(ast:decl name super fields methods)
     (let* ([class-name (ast:var-name name)]
            [super-name (ast:var-name super)]
            [field-names (append-field-names (class-field-name (lookup-class super-name)) fields)]
            [method-decls (merge-method-envs (class-method-env (lookup-class super-name)) (method-decls-method-env methods super-name field-names))])
       (add-to-class-env class-name (class super-name field-names method-decls)))]))


; append field name -> pg 344/20
(define (append-field-names super-fields new-fields)
  (let ([fields (map (lambda (field) (if (string? field) field (ast:var-name field))) new-fields)])
  (cond
    ((null? super-fields) fields)
    (else
     (cons
      (if (memq (car super-fields) new-fields)
          (append-field-names (car super-fields) new-fields)
          (car super-fields))
      (append-field-names
       (cdr super-fields) new-fields))))))

; find method -> pg 345/21
(define (find-method class-name name)
  (let ([m-env (class-method-env (lookup-class class-name))])
    (let ([maybe-pair (assoc name m-env)])
      (if (pair? maybe-pair) (cadr maybe-pair)
          (raise-user-error "report-method-not-found" name)))))


; method decls method env -> pg 345/21
(define (method-decls-method-env decls super-name field-names)
  (map
   (lambda (decl)
     (match decl
       ([ast:method method-name vars body]
        [list (ast:var-name method-name) (method (map ast:var-name vars) body super-name field-names)])))
   decls))


;merge method -> pg 345/21
(define (merge-method-envs super-env new-env)
  (append new-env super-env))
        
         
;values of exps
(define (values-of-exps exp Δ)
  (map (lambda (exp) (value-of exp Δ)) exp))




