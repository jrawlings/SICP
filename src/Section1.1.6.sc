// http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#%_sec_1.1.6

/*
 * Exercise 1.1.  Below is a sequence of expressions. What is the result
 * printed by the interpreter in response to each expression? Assume that
 * the sequence is to be evaluated in the order in which it is presented.
 */
// 10
10

// (+ 5 3 4)
5.+(3).+(4)

// (- 9 1)
9.-(1)

// (/ 6 2)
6./(2)

// (+ (* 2 4) (- 4 6))
2.*(4).+(6.-(4))

// (define a 3)
val a = 3

// (define b (+ a 1))
val b = a.+(1)

// (+ a b (* a b))
a+b+(a*b)

// (= a b)
a == b

// (if (and (> b a) (< b (* a b)))
//     b
//     a)
if(b > a && b < (a*b)) b else a

// (cond ((= a 4) 6)
//       ((= b 4) (+ 6 7 a))
//       (else 25))
if(a == 4) 6 else if(b == 4) 6 + 7 + a else 25

// (+ 2 (if (> b a) b a))
2 + (if(b > a) b else a)

// (* (cond ((> a b) a)
//          ((< a b) b)
//          (else -1))
//    (+ a 1))
(if(a > b) a else if(a < b) b else -1) * (a + 1)

/*
 * Exercise 1.2.  Translate the following expression into prefix form
 */
val answer = "(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))"

/*
 * Exercise 1.3.  Define a procedure that takes three numbers as arguments
 * and returns the sum of the squares of the two larger numbers.
 */
{
  def squareOfLargestTwo(nums:List[Int]) =
    nums.sorted.takeRight(2).foldLeft(0)((acc, n) => acc + (n*n))

  squareOfLargestTwo(List(1,2,3))
}

/*
 * Exercise 1.4.  Observe that our model of evaluation allows for combinations
 * whose operators are compound expressions. Use this observation to describe
 * the behavior of the following procedure:
 *
 * (define (a-plus-abs-b a b)
 *   ((if (> b 0) + -) a b))
 */
{
  def add = (a:Int, b:Int) => a + b
  def sub = (a:Int, b:Int) => a - b
  def a_plus_abs_b(a:Int, b:Int) = (if(b > 0) add else sub)(a, b)
  a_plus_abs_b(1,-2)
}



/*
 * Exercise 1.5.  Ben Bitdiddle has invented a test to determine whether the
 * interpreter he is faced with is using applicative-order evaluation or
 * normal-order evaluation. He defines the following two procedures:
 *
 * (define (p) (p))
 *
 * (define (test x y)
 *   (if (= x 0)
 *       0
 *       y))
 *
 * Then he evaluates the expression
 *
 * (test 0 (p))
 *
 * What behavior will Ben observe with an interpreter that uses
 * applicative-order evaluation? What behavior will he observe with an
 * interpreter that uses normal-order evaluation? Explain your answer.
 * (Assume that the evaluation rule for the special form if is the same
 * whether the interpreter is using normal or applicative order: The
 * predicate expression is evaluated first, and the result determines
 * whether to evaluate the consequent or the alternative expression.)
 */
{
  def p:Int = p
  def test_call_by_name(x: => Int, y: => Int)= if(x == 0) 0 else y
  def test_call_by_value(x:Int, y:Int)= if(x == 0) 0 else y

  test_call_by_name(0, (p))
//  test_call_by_value(0, (p)) // infinite loop
}
