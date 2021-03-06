/*
 * Exercise 1.6.  Alyssa P. Hacker doesn't see why if needs to be provided
 * as a special form. ``Why can't I just define it as an ordinary procedure
 * in terms of cond?'' she asks. Alyssa's friend Eva Lu Ator claims this can
 * indeed be done, and she defines a new version of if:
 *
 * (define (new-if predicate then-clause else-clause)
 *   (cond (predicate then-clause)
 *         (else else-clause)))
 *
 * Eva demonstrates the program for Alyssa:
 *
 * (new-if (= 2 3) 0 5)
 * 5
 *
 * (new-if (= 1 1) 0 5)
 * 0
 *
 * Delighted, Alyssa uses new-if to rewrite the square-root program:
 *
 * (define (sqrt-iter guess x)
 *   (new-if (good-enough? guess x)
 *           guess
 *           (sqrt-iter (improve guess x)
 *                      x)))
 *
 * What happens when Alyssa attempts to use this to compute square roots?
 * Explain.
 */
val a = """
           Since Scheme uses applicative order evaluation, all parameters
           of a function are evaluated before the function is executed. This
           results in an infinite loop. 'cond' and the special form 'if'
           evaluate predicates as-needed which result in a proper execution
           of the sqrt-iter function.

           This can be somewhat simulated in scala with a call-by-value
           new_if function. The new_if function below is call-by-name
           and works as intended.
"""
case class IsTrue(bool: Boolean) extends Function[Boolean, Boolean] {
  def apply(arg: Boolean) = arg
}
def sqrt_good_enough(a:Int, b:Int) = /* do something interesting */ a == b
def sqrt_iter(a:Int, b:Int): Int = /* do something interesting */ sqrt_iter(a, b)
def new_if(pred:IsTrue, then_clause: => Int, else_clause: => Int) =
  if(pred.bool) then_clause else else_clause

new_if(IsTrue(sqrt_good_enough(5, 5)), 5, sqrt_iter(5, 1))

/*
 * Exercise 1.7.  The good-enough? test used in computing square roots will
 * not be very effective for finding the square roots of very small numbers.
 * Also, in real computers, arithmetic operations are almost always performed
 * with limited precision. This makes our test inadequate for very large
 * numbers. Explain these statements, with examples showing how the test fails
 * for small and large numbers. An alternative strategy for implementing
 * good-enough? is to watch how guess changes from one iteration to the next
 * and to stop when the change is a very small fraction of the guess. Design
 * a square-root procedure that uses this kind of end test. Does this work
 * better for small and large numbers?
 */


/*
 * Exercise 1.8.  Newton's method for cube roots is based on the fact that
 * if y is an approximation to the cube root of x, then a better approximation
 * is given by the value
 *
 *
 * Use this formula to implement a cube-root procedure analogous to the
 * square-root procedure. (In section 1.3.4 we will see how to implement
 * Newton's method in general as an abstraction of these square-root and
 * cube-root procedures.)
 */
def cuberoot_good_enough(guess:Double, x:Double) = math.abs(math.pow(guess, 3) - x) < 0.001

def cuberoot_improve(guess:Double, x:Double) = (x / math.pow(guess, 2) + 2 * guess) / 3

def cuberoot_iter(guess:Double, x:Double): Double =
  if(cuberoot_good_enough(guess, x))
    guess
  else
    cuberoot_iter(cuberoot_improve(guess, x), x)

def cuberoot(x: Double) = cuberoot_iter(1.0, x)

cuberoot(27.0)