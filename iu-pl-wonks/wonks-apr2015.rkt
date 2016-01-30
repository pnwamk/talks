#lang slideshow 
(require slideshow/code)

;; title slide
(slide (bt "Enriching Typed Racket with Dependent Types")
       (t "Overview and Status Report")
       (it "Andrew M. Kent and Sam Tobin-Hochstadt")
       (text "Presented at PL Wonks @ Indiana University, 17 Apr 2015" '()  12))

;; how is this novel?
(slide #:title "The Big Picture"
       'next
       'alts
       (list (list (item "Typed Racket"))
             (list (item "Typed Racket + refinement types"))
             (list (item "Typed Racket + refinement types + linear integer constraints")))
       'next
       (item (it "Q:  Haven't those things been done before?"))
       'next
       (item "Individually? Absolutely!")
       'next
       (subitem (bt "sound interoperation") "with full featured dynamically typed language")
       'next
       (subitem (bt "unique type system") "successfully used by Typed Racket and Typed Clojure") 
       'next 
       'alts
       (list (list (subitem "no dependence on SMT solver"))
             (list (subitem "no dependence on SMT solver" 
                            (it "(more traditional type system)")))))

;; refresher
(slide #:title "Already Logical Types" 
       (t "A Brief Refresher on How Typed Racket Works!"))


;; Introduce the need for logical propositions
(slide #:title "Already Logical Types"
       (item "Question:  How do we type a Lisp-like language?")
       'next
       'alts
       (list (list (item "Simple example:")
                   'next
                   (subitem (code (+ 1 2)))
                   'next
                   (subitem (code +) "is of type" (code Number -> Number -> Number))
                   'next
                   (subitem (code 1) "and" (code 2) "are of type" (code Number))
                   'next
                   (subitem "therefore " (code (+ 1 2)) "is of type" (code Number)))
             (list (item "Less simple example:")
                   'next
                   (subitem (code (define (plus1 n)
                                    (if (fixnum? n)
                                        (fx+ n 1)
                                        (fl+ n 1.0)))))
                   'next
                   (subitem (code n) "is" (it "either") 
                            "a" (code Fixnum) "or a" (code Flonum))
                   'next
                   (subitem "What does" (code (fixnum? n)) "tell us?")))
       'next
       (item "Answer: We need" (bt "logical propositions") " about types!"))


;; Walk through logical propositions on plus1
(slide #:title "Already Logical Types"
       (item "Typechecking our example:")
       (code (define (plus1 n)
               (if (fixnum? n)
                   (fx+ n 1)
                   (fl+ n 1.0))))
       'next
       'alts
       (list (list (item "Assume" (code (n -: Fixnum)) 
                         (it "or") (code (n -: Float))))
             (list (item "Assume" (code (n -: (U Fixnum Float))))))
       'next
       'alts
       (list (list (item (bt "then branch") "...?"))
             (list (item (bt "then branch") (code (n -: Fixnum)))))
       'next
       (subitem (code (fx+ n 1)) "typechecks!")
       'next
       'alts
       (list (list (item (bt "else branch") "...?"))
             (list (item (bt "else branch") (code (n -! Fixnum))))
             (list (item (bt "else branch") 
                         (code (n -! Fixnum))
                         (code ⇒ (n -: Float)))))
       'next
       (subitem (code (fl+ n 1.0)) "typechecks!"))

;; Briefly show typing judgment
(slide #:title "Already Logical Types"
       (item "Typechecking our example:")
       (code (define (plus1 n)
               (if (fixnum? n)
                   (fx+ n 1)
                   (fl+ n 1.0))))
       'alts
       (list (list (item "Simple types won't cut it!")
                   'next
                   (subitem (code (Γ ⊢ (fixnum? n) 
                                     : Boolean))))
             (list (item "We need logical types!")
                   'next
                   (subitem (code (Γ ⊢ (fixnum? n) 
                                     : Boolean 
                                     \; when not #f => (n -: Fixnum) 
                                     \; when #f     => (n -! Fixnum)
                                     \; ∅)))))
       'next
       (item "Typed Racket uses logical propositions as part" 
             "of the typing judgement and in function types"))


;; refresher
(slide #:title "More Descriptive Types"
       (t "So what is the type of plus1?")
       'next
       (t "(Hint... we're going to leverage those logical propositions!)"))

;; so what is the type of plus1?
;; Briefly show typing judgment
(slide #:title "More Descriptive Types"
       (item "The type of" (code plus1) ":")
       'next
       'alts
       (list 
        (list (item "Good")
              (code [(U Fixnum Float) -> (U Fixnum Float)])
              'next
              (item "But we'd like to know" (code (plus1 1)) "produces a" (code Fixnum)))
        (list (item "Better")
              (code (case-> [Fixnum -> Fixnum]
                            [Float -> Float]
                            [(U Fixnum Float) 
                             -> (U Fixnum Float)]))
              'next
              (item "But we don't want to forget the relation between input and output types!"))
        (list (item "e.g. we want this to typecheck:" 
                    (code (define (foo [x : (U Fixnum Float)]) : Fixnum
                            (let ([y (plus1 x)])
                              (if (fixnum? x)
                                  (fx* x y)
                                  42)))))
              'next
              (item "current type of" (code plus1) ":")
              (code (case-> [Fixnum -> Fixnum]
                            [Float -> Float]
                            [(U Fixnum Float) 
                             -> (U Fixnum Float)])))
        (list (item "Best")
              (code (dependent-case-> 
                     [Fixnum -> Fixnum]
                     [Float -> Float]))
              'next
              'alts
              (list (list (item "Now this typechecks!"
                                (code (define (foo [x : (U Fixnum Float)]) : Fixnum
                                        (let ([y (plus1 x)])
                                          (if (fixnum? x)
                                              (fx* x y)
                                              42))))))
                    (list (item "... but how" (it "is") (code dependent-case->) "implemented?"))))
        (list (item "Best")
              (code (dependent-case-> 
                     [Fixnum -> Fixnum]
                     [Float -> Float]))
              'alts
              (list (list (item "expands to:")
                          (code [([x : (U Fixnum Float)])
                                 -> 
                                 (Refine [ret : (U Fixnum Float)]
                                         (or (and [x   -: Fixnum]
                                                  [ret -: Fixnum])
                                             (and [x   -: Float]
                                                  [ret -: Float])))]))
                    (list (item "refinement types = more precise types!")
                          (code [([x : (U Fixnum Float)])
                                 -> 
                                 (Refine [ret : (U Fixnum Float)]
                                         (or (and [x   -: Fixnum]
                                                  [ret -: Fixnum])
                                             (and [x   -: Float]
                                                  [ret -: Float])))]))))))

(slide #:title "More Descriptive Types"
       (code [([x : (U Fixnum Float)])
              -> 
              (Refine [ret : (U Fixnum Float)]
                      (or (and [x   -: Fixnum]
                               [ret -: Fixnum])
                          (and [x   -: Float]
                               [ret -: Float])))])
       (item "Sound interoperability?")
       'next
       'alts
       (list (list 'alts
                   (list (list (subitem "with typed code? "))
                         (list (subitem "with typed code? Typechecker!")))
                   'next
                   'alts
                   (list (list (subitem "with untyped code?"))
                         (list (subitem "with untyped code? Contracts!"))))
             (list (code (->i ([x (or/c fixnum? flonum?)])
                              [ret (x) (if (fixnum? x) fixnum? flonum?)])))))

(slide #:title "More Descriptive Types"
       'alts
       (list (list (item "Another example"))
             (list (item "#racket: prevent flonum divide-by-zero error")))
       'next
       (item "Assume" (code (denom -: Float)) 
             "and" (code (ε -: Positive-Float)))
       'next
       (code (cond
               [(fl> (flabs denom) ε) 
                <division-exp>]
               ...))
       'next
       (item "In" (code <division-exp>) "we know" (code denom ≠ 0) ", but" 
             "currently in TR this fact is lost")
       'next
       'alts
       (list (list (item (code (flabs denom)) "types at" (code Fixnum) "currently"))
             (list (item "With a dependent refinement we could more accurately track these types!")))
       'next
       (subitem "Just like" (code plus1) "using" (code dependent-case->) "to better track types"))


(slide #:title "Linear Integer Constraints"
       (t "Relating more than just 'types'!"))

(slide #:title "Linear Integer Constraints"
       (item "Types can depend on other types...")
       'next
       (item "What about other practical, decidable theories?")
       'next
       (item (bt "Linear integer constraints") "are a well" 
             "understood, decidable problem w/ numerous applications!"))

(slide #:title "Linear Integer Constraints"
       'alts
       (list (list (code (define (norm [v : (Vectorof Real)])
                           (sqrt (for/sum ([i (vec-len v)])
                                   (square (vector-ref v i))))))
                   'next
                   (item "Is it possible to get an out of bounds error from"
                         (code (vector-ref v i)) "?")
                   'next
                   (item "Nope!" (code ∀i (and (≤ 0 i) (< i (vec-len v))))))
             (list (code (define (norm [v : (Vectorof Real)])
                           (sqrt (for/sum ([i (vec-len v)])
                                   (square (vector-ref v i))))))
                   (item "The optimizer can replace" (code vector-ref)
                         "with" (code unsafe-vector-ref) ":")
                   'next
                   (item "This requires no intervention from the user!"))))

(slide #:title "Linear Integer Constraints"
       (code (: safe-vector-ref 
                (All (α) (([v : (Vectorof α)]
                           [i : Natural (< i (vec-len v))])
                          -> α)))
             (define safe-vector-ref vector-ref))
       'next
       (item "Users can specifically require statically guaranteed safe usages of functions like" (code vector-ref))
       'next
       (item (code safe-vector-ref) "can never have a runtime out-of-bounds error!"))

(slide #:title "Linear Integer Constraints"
       (code (define (dot-product [v1 : (Vectorof Real)]
                                  [v2 : (Vectorof Real)
                                      (= (vec-len v1)
                                         (vec-len v2))])
               (for/sum ([i (vec-len v1)])
                 (* (safe-vector-ref v1 i)
                    (safe-vector-ref v2 i)))))
       'next
       (item "No bounds errors + verified optimizations!"))

(slide #:title "Linear Integer Constraints"
       (item "What about" (code plus1) "?")
       'next
       (code (dependent-case->
              [[n : Fixnum] -> [m : Fixnum
                                  (= m (+ 1 n))]]
              [Float -> Float])))


(slide #:title "Recap"
       (item "Refinements are a great, natural extension to Typed Racket!")
       'next
       (subitem "Relate the types of runtime values and reason about common integer constraints!")
       'next
       (subitem "Create expressive dependent functions!")
       'next
       (subitem "The logical propositions are already" 
                "part of the type system!")
       'next
       (subitem "Refinements are easily mapped to dependent contracts")
       'next
       (subitem "Small/medium scale implemented and working! (Currently scaling to full scale...)"))

(slide #:title "To Do"
       (t "Next Steps?"))

(slide #:title "To Do"
       (item "Finish implementing these features")
       'next
       (item "Experiment w/ new types for standard library")
       'next
       (item "Support arbitrary" (it "pure") "predicates in refinements")
       'next
       (t "Thanks!")
       (item "PLT Redex model available:" (tt "https://github.com/andmkent/stop2015-redex"))
       (item "Typed Racket fork:" (tt "https://github.com/andmkent/typed-racket"))
       (subitem "Work in progress!  =)"))