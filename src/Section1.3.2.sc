/**
 * Exercise 1.34.  Suppose we define the procedure
 *
 * (define (f g)
 *   (g 2))
 *
 * Then we have
 *
 * (f square)
 *   4
 *
 * (f (lambda (z) (* z (+ z 1))))
 *   6
 *
 * What happens if we (perversely) ask the interpreter to evaluate the combination (f f)?
 * Explain.
 */
def square(x:Int) = x * x
def f(g:Function[Int, Int]) = g(2)
f(square)

f((z) => z * (z + 1))

// Type mismatch, expect (Int) => Int, actual ((Int) => Int) => Int
// f(f)

/**
 * (f f)
 * (f 2)
 * (2 2)
 */