
def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a>b) acc
    else loop(a+1, f(a)+acc)
  }
  loop(a, 0)
}
sum( x => x*x,1,4)

def sum2(f:Int => Int): (Int,Int) => Int = {
  def innard(a:Int, b:Int):Int = {
    if (a>b) 0
    else f(a) + innard(a+1,b)
  }
  innard
}
def sumsquare= sum2( x => x*x)
sumsquare(1,4)

def sum3(f:Int => Int)(a:Int, b:Int) :Int = {
  if (a>b) 0
  else f(a) + sum3(f)(a+1,b)
}
sum3(x => x*x)(1,4)
// why doesnt this work?
//def bob = sum3( x => x*x)
//bob(1,4)
