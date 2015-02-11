/**
 * Exercise 1.42.  Let f and g be two one-argument functions. The composition f after g is
 * defined to be the function x  f(g(x)). Define a procedure compose that implements composition.
 * For example, if inc is a procedure that adds 1 to its argument,
 *
 * ((compose square inc) 6)
 *   49
 */
def square(x:Int) = x * x
def inc(x:Int) = x + 1
def compose(f:Function[Int, Int], g:Function[Int, Int]) = f compose g // g andThen f
compose(square, inc)(6)

/**
 * Exercise 1.43.  If f is a numerical function and n is a positive integer, then we can form
 * the nth repeated application of f, which is defined to be the function whose value at x is
 * f(f(...(f(x))...)). For example, if f is the function x |-> x + 1, then the nth repeated
 * application of f is the function x |-> x + n. If f is the operation of squaring a number,
 * then the nth repeated application of f is the function that raises its argument to the 2nth
 * power. Write a procedure that takes as inputs a procedure that computes f and a positive
 * integer n and returns the procedure that computes the nth repeated application of f. Your
 * procedure should be able to be used as follows:
 *
 * ((repeated square 2) 5)
 *   625
 *
 * Hint: You may find it convenient to use compose from exercise 1.42.
 */
def f(x:Int) = x + 1
def repeated(f:Function[Int, Int], n:Int):Function[Int, Int] =
  if(n == 1) f else compose(f, repeated(f, n - 1))
repeated(square, 2)(5)
