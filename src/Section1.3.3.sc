/**
 * Exercise 1.35.  Show that the golden ratio phi (section 1.2.2) is a fixed point
 * of the transformation x |-> 1 + 1/x, and use this fact to compute phi by means of
 * the fixed-point procedure.
 */
val tolerance = 0.00001
def fixed_point(f:Function[Double, Double], first_guess:Double) = {
  def close_enough(v1:Double, v2:Double) = Math.abs(v1 - v2) < tolerance
  def try_guess(guess:Double):Double = {
    val next = f(guess)
    if(close_enough(guess, next)) next else try_guess(next)
  }
  try_guess(first_guess)
}

fixed_point((x) => 1 + 1/x, 1)
