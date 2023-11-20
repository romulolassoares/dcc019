#lang racket

(require dcc019/util/env
         dcc019/util/memory
         dcc019/exercise/iclasses/ast)

(provide value-of-program)

; ESTADOS INICIAIS =======================================
; Ambiente de classes inicializa vazio.
(define class-env '())
; ESTADOS INICIAIS =======================================

; STRUCTS =======================================
(struct class (super-name field-names method-env))
(struct method (vars body super-name fields))
(struct object (class-name fields))
; STRUCTS =======================================

; ENV =======================================
; Função para inicializar o ambiente de classes
(define (initialize-class-env decls)
  (begin
    ; Super classe do programa é a object. A classe object não possui uma superclasse, então é atribuído
    ; falso a ela.
    (add-class-to-env "object" (class #f '() '()))
    (add-classes-from-decls decls)

    ; DEBUG
    ;(display class-env)
  )
)

; Função para adicionar nova classe ao ambiente
(define (add-class-to-env class-name class-list)
  (set! class-env (cons (cons class-name class-list) class-env)))

; Função para adicionar as classes de decl ao ambiente
(define (add-classes-from-decls decls)
  (for ([decl decls])
    (let* ([name (ast:var-name (ast:decl-name decl))]
           [super-name (ast:var-name (ast:decl-super decl))]
           [super-fields (class-field-names (find-class super-name))]
           [fields (append-field-names super-fields (get-field-names (ast:decl-fields decl)))]
           [methods (merge-method-envs (ast:decl-methods decl) super-name fields)])
           (add-class-to-env name (class super-name fields methods))))
  1)

; Função para procurar classe no ambiente
(define (find-class class-name)
  (cdr (assoc class-name class-env)))

; Função para retornar uma lista de strings dos nomes dos campos de uma classe
(define (get-field-names fields)
  (map (lambda (field) (if (string? field) field (ast:var-name field))) fields)
)

; Função para concatenar campos da classe com a classe pai
(define (append-field-names super-fields self-fields)
  (if (empty? super-fields)
      self-fields
      (if (member (car super-fields) self-fields)
          (append-field-names (cdr super-fields) (append self-fields (list (string-append (car super-fields) "%1"))))
          (append-field-names (cdr super-fields) (append self-fields (list (car super-fields)))))))

; Função para combinar os ambientes dos métodos
(define (merge-method-envs m-decls super-name fields)
  (append
   (map (lambda (m-decl) (create-method super-name fields m-decl)) m-decls)
   (class-method-env (find-class super-name))))

; Função para criar método com a struct definida a partir dos parâmetros
(define (create-method super-name fields m-decl)
  (list
   (ast:var-name (ast:method-name m-decl))
   (method (map ast:var-name (ast:method-params m-decl)) (ast:method-body m-decl) super-name fields)
   )
)

; Função para avaliar o método no ambiente com as variáveis, os campos e os valores das variáveis self e super
(define (apply-method method self args )
  (result-of
    (method-body method)
    (bind-vars-to-env
      (method-vars method)
      (map newref args)
      (bind-vars-to-env
        (method-fields method)
        (object-fields self)
        (extend-env "self" self
                  (extend-env "super" (method-super-name method)
                              empty-env)))
      )))

; Função para atrelar determinados vars e values a um ambiente
(define (bind-vars-to-env vars values env)
  (for ([var vars] [val values])
    (set! env (extend-env var val env)))
  env)

; Função para encontrar um método no ambiente
(define (find-method class-name method-name )
  (let ([m-env (class-method-env (find-class class-name))])
    (let ([maybe-pair (assoc method-name m-env)])
      (if (pair? maybe-pair)
          (cadr maybe-pair)
          (raise-user-error "method-not-found" method-name)))))
; ENV =======================================

; UTILS =======================================
; Função lambda para retornar o value-of de cada expressão
(define map-value-of
  (lambda (exps Δ)
    (map (lambda (exp) (value-of exp Δ)) exps)
  )
)

; Função lambda para iniciar uma nova instância de um objeto
(define (new-object-instance class-name )
  (object
    class-name
    (map (lambda (field-name) (newref null)) (class-field-names (find-class class-name)))
  )
)
; UTILS =======================================

; EXECUTE =======================================
; Executa a regra 'send'
(define (execute-send object-exp method-name args Δ)
  (let* ([args-with-value (map-value-of args Δ )]
         [obj (value-of object-exp Δ )])
    (apply-method (find-method (object-class-name obj) method-name) obj args-with-value )))

; Executa a regra 'new'
(define (execute-new-object class-name args Δ)
  (let* ([args-with-value (map-value-of args Δ )]
         [obj (new-object-instance class-name )])
    (apply-method (find-method class-name "initialize") obj args-with-value )
    obj))

; Executa a regra 'self'
(define (execute-self Δ) (apply-env Δ "self"))

; Executa a regra 'super'
(define (execute-super method-name args Δ)
  (let ([args-with-value (map-value-of args Δ )]
           [obj (apply-env Δ "self")])
    (apply-method (find-method (apply-env Δ "super") (ast:var-name method-name)) obj args-with-value )))
; EXECUTE =======================================

; value-of :: Exp -> ExpVal
(define (value-of exp Δ)
  (match exp
    [(ast:int v) v]
    [(ast:bool v) v]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:not e) (not (value-of e Δ))]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    [(ast:var v) (deref (apply-env Δ v))]
    [(ast:let (ast:var x) e1 e2) (value-of e2 (extend-env x (value-of e1 Δ) Δ))]
    [(ast:send e (ast:var mth) args) (execute-send e mth args Δ)]
    [(ast:super (ast:var c) args) (execute-super c args Δ)]
    [(ast:self) (execute-self Δ)]
    [(ast:new (ast:var c) args) (execute-new-object c args Δ)]
    [e (raise-user-error "unimplemented-value-of-construction: " e)]
    ))

; result-of :: Stmt -> Env -> State -> State
(define (result-of stmt Δ)
  (match stmt
    [(ast:assign (ast:var x) e) (begin (setref! (apply-env Δ x) (value-of e Δ)) 42)]
    [(ast:print e) (display (value-of e Δ))]
    [(ast:return e) (value-of e Δ)]
    [(ast:block stmts) (for ([s stmts]) (result-of s Δ))]
    [(ast:if-stmt e s1 s2) (if (value-of e Δ) (result-of s1 Δ) (result-of s2 Δ))]
    [(ast:while e s) (if (value-of e Δ)
                         (begin
                           (result-of s Δ)
                           (result-of stmt Δ))
                         'done)]
    [(ast:local-decl (ast:var x) s) (result-of s (extend-env x (newref 'null) Δ))]
    [(ast:send e (ast:var mth) args) (execute-send e mth args Δ)]
    [(ast:super (ast:var c) args) (execute-super c args Δ)]
    [e (raise-user-error "unimplemented-result-of-construction: " e)]
    ))

(define (value-of-program prog)
  (empty-store)
  (match prog
    [(ast:prog decls stmt)
     (begin
       (initialize-class-env decls)
       (result-of stmt init-env))]))
