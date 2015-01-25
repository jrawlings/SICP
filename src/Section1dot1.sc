// Exercise 1.1
10
5.+(3).+(4)
9.-(1)
6./(2)
(2.*(4)).+(6.-(4))
val a = 3;
val b = a.+(1)
a+b+(a*b)
a == b
if(b > a && b < (a*b)) b else a
// (cond (...
2 + (if(b > a) b else a)
// (* (cond ((...

// Exercise 1.2
val answer = "(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))"

// Exercise 1.3
def squareOfLargestTwo(nums:List[Int]) =
  nums.sorted.takeRight(2).foldLeft(0)((acc, n) => acc + (n*n))

squareOfLargestTwo(List(1,2,3))

// Exercise 1.4