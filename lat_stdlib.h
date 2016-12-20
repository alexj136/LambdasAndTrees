// The Lambdas and Trees Standard Library
// To use, include this file in source code and run the c preprocessor with
//
//     cpp -P infile -o outfile

#ifndef stdlib
#define stdlib

// ---------------------------------
// Boolean logic
// ---------------------------------

#define true (nil.nil)
#define false nil

#define and ( | a : @ . | b : @ . \
    if a then \
        if b then true else false end \
    else \
        false \
    end \
)

#define or ( | a : @ . | b : @ . \
    if a then \
        true \
    else \
        if b then true else false end \
    end \
)

#define not ( | a : @ . if a then false else a end )

// ---------------------------------
// Arithmetic
// ---------------------------------

#define oneplus ( | x : @ . ( nil . x ) )

#define zero nil

#define add(x, y) ( letrec ( doAdd : @ -> @ -> @ , ( | xa : @ . ( | ya : @ . \
    if xa then \
        doAdd (> xa) (nil.ya) \
    else \
        ya \
    end ) )\
    , ( doAdd ( x ) ( y ) ) \
) )

// ---------------------------------
// Let expressions
// ---------------------------------

#define let(x, exp, in) ( ( | x . in ) exp )

#define letrec(x, exp, in) ( let(x, ( Y ( | x . exp ) ), in) )

#endif
