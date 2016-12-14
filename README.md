### Lambdas and Trees

Lambdas and Trees is programming language with lambdas and binary trees.

##### Syntax

**Expressions:**

    M, N ::=    | x . M         Abstraction
          |     | x: T . M      Labelled abstration
          |     M N             Application
          |     Y M             Y-combinator application
          |     nil             Empty tree
          |     (M.N)           Tree constructor
          |     < M             Right tree destructor
          |     > M             Left tree destructor
          |     (M)             Parenthesised expression

**Types:**

    T    ::=    @               Trees
          |     T -> T          Functions
          |     (T)             Parenthesised type

**Variables:**

    x   ::= [a-zA-Z][a-zA-Z_]*
