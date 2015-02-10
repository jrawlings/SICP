/**
 * Exercise 1.31. a.  The sum procedure is only the simplest of a vast number of
 * similar abstractions that can be captured as higher-order procedures. Write
 * an analogous procedure called product that returns the product of the values
 * of a function at poDoubles over a given range. Show how to define factorial in
 * terms of product. Also use product to compute approximations to pi using the
 * formula
 *
 * b.  If your product procedure generates a recursive process, write one that generates
 * an iterative process. If it generates an iterative process, write one that generates
 * a recursive process.
 */
def identity(x:Double) = x
def inc_any(x:Double) = (a:Double) => a + x

def prod(term:Function1[Double, Double], a:Double, next:Function1[Double, Double], b:Double):Double =
  if(a > b) 1 else term(a) * prod(term, next(a), next, b)
def product(a:Double, b:Double) = prod(identity, a, inc_any(1), b)
product(2,4)

def factorial(a:Double) = prod(identity, 1, inc_any(1), a)
factorial(4)

def pi(a:Double) = {
  def term(a:Double) = ((a - 1.0) / a) * ((a + 1.0) / a)
  4.0 * prod(term, 3, inc_any(2), a)
}
pi(100)

def prod_iter(term:Function1[Double, Double], a:Double, next:Function1[Double, Double], b:Double) = {
  def iter(a:Double, result:Double):Double =
    if(a > b) result else iter(next(a), result * term(a))
  iter(a, 1)
}
def product_iter(a:Double, b:Double) = prod_iter(identity, a, inc_any(1), b)
product_iter(2,4)

/**
 * Exercise 1.32.  a. Show that sum and product (exercise 1.31) are both special cases
 * of a still more general notion called accumulate that combines a collection of terms,
 * using some general accumulation function:
 *
 * (accumulate combiner null-value term a next b)
 *
 * Accumulate takes as arguments the same term and range specifications as sum and
 * product, together with a combiner procedure (of two arguments) that specifies how
 * the current term is to be combined with the accumulation of the preceding terms
 * and a null-value that specifies what base value to use when the terms run out.
 * Write accumulate and show how sum and product can both be defined as simple calls
 * to accumulate.
 */
def accumulate(combiner:Function2[Double, Double, Double], null_value:Double,
               term:Function1[Double, Double], a:Double, next:Function1[Double, Double],
               b:Double):Double = {
  if(a > b) null_value else combiner(term(a), accumulate(combiner, null_value, term, next(a), next, b))
}

def prod_acc(a:Double, b:Double) = accumulate((x:Double, y:Double) => x * y, 1, identity, a, inc_any(1), b)

prod_acc(2, 5)