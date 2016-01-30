#lang racket

;; This is a modified version of the STLC example
;; that comes w/ the redex language.
;; Modified by Andrew Kent, Jan '16

(require redex)

(define-language stlc++
  [x   ::= variable-not-otherwise-mentioned]
  [n   ::= number]
  [b   ::= true false]
  [bop ::= + - * and or]
  [v   ::= n b (λ (x τ) e)]
  [e   ::= v x (e e) (bop e e) (if e e e)]
  [τ   ::= (τ -> τ) (τ τ -> τ) num bool]
  [C   ::= hole (v C) (C e)
       (if C e e) (bop C e) (bop v C)]
  [Γ   ::= ∅ (x τ Γ)]
  ;; after [  ]
  #:binding-forms
  (λ (x τ) e #:refers-to x))

;; substitution
(define-metafunction stlc++
  subst : e [x ↦ v] -> e
  [(subst e [x ↦ v]) (substitute e x v)])


;; α-equivalence for stlc++
(define (α=? term1 term2)
  (alpha-equivalent? stlc++ term1 term2))

;; α-equivalence for stlc++
(test-equal (term (λ (x num) x))
            (term (λ (y num) y))
            #:equiv α=?)


;; δ
(define-metafunction stlc++
  δ : bop v v -> v
  [(δ + n_1 n_2) ,(+ (term n_1) (term n_2))]
  [(δ - n_1 n_2) ,(- (term n_1) (term n_2))]
  [(δ and true true) true]
  [(δ and b_1 b_2) false]
  [(δ or true b) true]
  [(δ or b true) true]
  [(δ or b_1 b_2) false])

;; reduction relation 'red'
(define red
  (reduction-relation
   stlc++
   (--> (in-hole C (bop v_1 v_2))
        (in-hole C v)
        (where v (δ bop v_1 v_2))
        "δ")
   (--> (in-hole C (if true e_1 e_2))
        (in-hole C e_1)
        "if-true")
   (--> (in-hole C (if false e_1 e_2))
        (in-hole C e_2)
        "if-false")
   (--> (in-hole C ((λ (x τ) e) v))
        (in-hole C (subst e [x ↦ v]))
        "βv")))



;; red tests
(test-->> red (term ((λ (x num) x) 1)) 1)
(test-->> red (term (((λ (x num) (λ (y num) x)) 1) 2)) 1)
(test-->> red (term (((λ (x num) (λ (y num) y)) 1) 2)) 2)
(test-->> red (term (((λ (x num) (λ (x num) x)) 1) 2)) 2)
(test-->> red (term (((λ (x num) (λ (y num) x)) 1) 2)) 1)
(test-->> red (term ((λ (x num) (+ x x)) 2)) 4)
(test-->> red (term ((λ (b num) (if b 2 1)) true)) 2)
(test-->> red
          #:equiv α=?
          (term (((λ (x num) (λ (y num) (λ (z num) x))) 1) 2))
          (term (λ (z num) 1)))
(test-->> red
          (term (+ (+ 1 2) (+ 3 4)))
          (term 10))


;; δτ
(define-metafunction stlc++
  δτ : bop -> τ
  [(δτ +) (num num -> num)]
  [(δτ -) (num num -> num)]
  [(δτ and) (bool bool -> bool)]
  [(δτ or) (bool bool -> bool)])


;; lookup
(define-metafunction stlc++
  lookup : Γ x -> τ or #f
  [(lookup (x τ Γ) x) τ]
  [(lookup (x_1 τ Γ) x_2) (lookup Γ x_2)]
  [(lookup ∅ x) #f])


;; typeof
(define-judgment-form stlc++
  #:mode (typeof I I O)
  #:contract (typeof Γ e τ)
  
  [--------------------- "T-Num"
   (typeof Γ n num)]

  [--------------------- "T-Bool"
   (typeof Γ b bool)]
  
  [(where (τ_1 τ_2 -> τ) (δτ bop))
   (typeof Γ e_1 τ_1)
   (typeof Γ e_2 τ_2)
   -------------------------- "T-Op"
   (typeof Γ (bop e_1 e_2) τ)]
  
  [(typeof Γ e_1 bool)
   (typeof Γ e_2 τ)
   (typeof Γ e_3 τ)
   ------------------------------ "T-If"
   (typeof Γ (if e_1 e_2 e_3) τ)]
  
  [(where τ (lookup Γ x))
   ---------------------- "T-Var"
   (typeof Γ x τ)]
  
  [(typeof Γ e_1 (τ_2 -> τ))
   (typeof Γ e_2 τ_2)
   -------------------------- "T-App"
   (typeof Γ (e_1 e_2) τ)]
  
  [(typeof (x_1 τ_1 Γ) e τ)
   ------------------------------------- "T-Abs"
   (typeof Γ (λ (x_1 τ_1) e) (τ_1 -> τ))])


(define (typecheck G e)
  (match (judgment-holds (typeof ,G ,e τ) τ)
    [(list) #f]
    [(list t) t]
    [_ (error 'typecheck
              "multiple typing derivations for term ~a in environment ~a"
              e G)]))
;; typecheck tests
(test-equal (typecheck (term ∅) (term 1))
            (term num))
(test-equal (typecheck (term ∅) (term (1 1)))
            #f)
(test-equal (typecheck (term (x num ∅)) (term x))
            (term num))
(test-equal (typecheck (term ∅) (term x))
            #f)
(test-equal (typecheck (term ∅) (term ((λ (x num) x) 1)))
            (term num))
(test-equal (typecheck (term ∅) (term (((λ (x num) x) 1) 2)))
            #f)
(test-equal (typecheck (term ∅) 
                       (term (((λ (f (num -> num))
                                 (λ (x num)
                                   (f x)))
                               (λ (x num) x))
                              1)))
            (term num))
(test-equal (typecheck (term ∅)
                       (term (((λ (f (num -> num))
                                 (λ (x num)
                                   (f x)))
                               1)
                              (λ (x num) x))))
            #f)
(test-equal (typecheck (term ∅) (term (+ (+ 1 2) 3)))
            (term num))
(test-equal (typecheck (term ∅) (term (if true (λ (x num) x) 3)))
            #f)
(test-equal (typecheck (term ∅) (term (if true 2 3)))
            (term num))
(test-equal (typecheck (term ∅) (term (λ (x num) (x 2))))
            #f)
(test-equal (typecheck (term ∅)
                       (term (λ (x num)
                               (λ (x (num -> num))
                                 (x 0)))))
            (term (num -> ((num -> num) -> num))))
(test-equal (typecheck (term ∅)
                       (term (λ (x (num -> num))
                               (λ (x num)
                                 (x 0)))))
            #f)


;; remove this #; to run an example

#;(traces red
          (term 
           (+ ((λ (b bool) (if b 1 0))
               true)
              (+ 2 2))))

;; remove this #; to generate a random well-typed term

#;(generate-term l #:satisfying (typeof ∅ e num) 5)

;; preservation
;; checks type preservation for a particular 'e'
(define (preservation e)
  (define types (judgment-holds (typeof ∅ ,e τ) τ))
  (unless (null? types)
    (unless (= 1 (length types)) (error 'preservation "multiple types! ~s" e)))
  (cond
    [(null? types) #t]
    [else
     (for/and ([v (apply-reduction-relation* red e)])
       (equal? (judgment-holds (typeof ∅ ,v τ) τ)
               types))]))

;; check a bunch of well typed terms for preservation
(define (try-it)
  (redex-check stlc++ 
               #:satisfying (typeof ∅ e num) 
               (preservation (term e))))



(test-results)
