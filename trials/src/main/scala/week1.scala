/**
  * Created by andy on 18/10/16.
  */
object week1 {
  def and(x:Boolean, y: => Boolean):Boolean = if (x) y else false

  and(true, true)
  and(true, false)
  and(false, true)
  and(false, false)

  def or(x:Boolean, y: => Boolean):Boolean = if (x) true else y


  def sqrt(x:Double):Double = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else (sqrtIter(improve(guess)))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.00001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    def abs(a: Double): Double = if (a < 0) -1 * a
    else a

    sqrtIter(1)
  }

  sqrt(4)
  sqrt(100)
  sqrt(343)
  sqrt(1e-20)
  sqrt(1e50)



  def factorial(n:Int):Int = {
    def fact(totalSoFar:Int, n:Int):Int = {
      if (n==0) totalSoFar
      else fact(totalSoFar*n,n-1)
    }
    fact(1,n)
  }

  factorial(4)

}
