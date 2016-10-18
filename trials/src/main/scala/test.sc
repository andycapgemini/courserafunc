
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

def product(f:Int => Int)(a:Int, b:Int):Int = {
  if (a>b) 1
  else f(a) * product(f)(a+1,b)
}
product(x => x*x)(1,3)

def factorial(n:Int) :Int = product(x => x)(1,n)

factorial(4)

//generalised function for product annd sum
// he called this a mapReduce
def general(combiner:(Int,Int)=>Int,identity:Int)(f:Int => Int,a:Int, b:Int) :Int = {
  if (a > b) identity
  else combiner(f(a), general(combiner, identity)(f, a + 1, b))
}
def fact(n:Int):Int = general((x:Int, y:Int)=> x*y,1)(x=> x,1,n)
def product(f:Int=>Int,a:Int, b:Int) = general((x:Int, y:Int)=> x*y,1)(f,a,b)
def sum4(f:Int=>Int,a:Int, b:Int) = general((x:Int, y:Int)=> x+y,0)(f,a,b)
fact(4)