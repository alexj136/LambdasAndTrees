# Lambdas and Trees

Lambdas and Trees is programming language with lambdas and binary trees.

## Syntax

**Expressions:**

    M, N, O ::=    | x . M                  Abstraction
             |     | x: T . M               Labelled abstration
             |     M N                      Application
             |     if M then N else O end   Conditional expression
             |     let x = M in N           Let-expression
             |     fix                      Fixed-point combinator
             |     nil                      Empty tree
             |     (M.N)                    Tree constructor
             |     < M                      Right tree destructor
             |     > M                      Left tree destructor
             |     (M)                      Parenthesised expression

**Types:**

    T, U    ::=    @                        Trees
             |     T -> U                   Functions
             |     a                        Type variables
             |     (T)                      Parenthesised type

**Types schemes:**

    σ       ::=    ∀a.σ                     Universal quantifier
             |     T                        Type

**Variables:**

    x       ::= [a-zA-Z][a-zA-Z_]*

## Typing Rules

Terms are typed according to the following rules:


            Γ(x) = T
    VAR     ━━━━━━━━
            Γ ⊢ x: T


            Γ ⊢ M : T -> U      Γ ⊢ N : T
    APP     ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                     Γ ⊢ M N : U


              Γ, x: T ⊢ M : U
    LLAM    ━━━━━━━━━━━━━━━━━━━
            Γ ⊢ |x:T.M : T -> U


             Γ, x: T ⊢ M : U
    ULAM    ━━━━━━━━━━━━━━━━━
            Γ ⊢ |x.M : T -> U


                Γ ⊢ N[M/x] : T
    LET     ━━━━━━━━━━━━━━━━━━━━━━
            Γ ⊢ let x = M in N : T


            Γ ⊢ M : @      Γ ⊢ N : T      Γ ⊢ O : T
    COND    ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                 Γ ⊢ if M then N else O end : T


    FIX     Γ ⊢ fix : ∀a. (a -> a) -> a


             Γ ⊢ M: @
    HD      ━━━━━━━━━━━
            Γ ⊢ hd M: @


             Γ ⊢ M: @
    TL      ━━━━━━━━━━━
            Γ ⊢ tl M: @


            Γ ⊢ M: @     Γ ⊢ N: @
    CONS    ━━━━━━━━━━━━━━━━━━━━━
                Γ ⊢ (M.N): @


    NIL     Γ ⊢ nil: @

Note that the let construct has ML-style let-polymorphism.

## Operational Semantics

We define the operational semantics in terms of a semantic function `S`, typed:

    S : term → (@ + ⊥)

`S` is defined over all well-typed closed terms. It is undefined over untypeable terms, terms with free variables, and terms that evaluate to `> nil` or `< nil`. Its definition is given as follows:


    S (|x:T.M)                 = S (|x.M)
    S (|x.M)                   = |x.M
    S ((|x.M) N)               = S (M[N/x])
    S (M N)                    = S ((S M) (S N))
    S (let x = M in N)         = S (N[M/x])
    S (fix M)                  = S (M (fix M))
    S (if M then N else O end) = S N if S M = nil
    S (if M then N else O end) = S O if S M = (P.Q)
    S (M.N)                    = (S M.S N)
    S (< (M.N))                = S M
    S (> (M.N))                = S N
    S nil                      = nil
    S (> nil)                  = ⊥
    S (< nil)                  = ⊥
    S x                        = ⊥
