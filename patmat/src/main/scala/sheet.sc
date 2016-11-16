object t { //stops forward reference errors in scala worksheets!


  abstract class IdealisedBoolean {
    def ifThenElse[T](t: => T, e: => T): T

    def &&(x: => IdealisedBoolean): IdealisedBoolean = ifThenElse(x, ifalse)
    def ||(x: => IdealisedBoolean): IdealisedBoolean = ifThenElse(itrue, x)
    def unary_! : IdealisedBoolean = ifThenElse(ifalse, itrue)
    def ==(x: IdealisedBoolean): IdealisedBoolean = ifThenElse(x, x.unary_!)
    def !=(x: IdealisedBoolean): IdealisedBoolean = ifThenElse(x.unary_!, x)
    def < (x: IdealisedBoolean): IdealisedBoolean = ifThenElse(ifalse,x)
  }

  object itrue extends IdealisedBoolean {
    def ifThenElse[T](t: => T, e: => T): T = t
    override def toString():String = "true"
  }

  object ifalse extends IdealisedBoolean {
    def ifThenElse[T](t: => T, e: => T): T = e
    override def toString():String = "false"
  }

  val a = itrue
  val b = ifalse

  a < b
  b < a
  a == a
  a == b

}

// piano numbers!
  abstract class Nat {
    def isZero: Boolean
    def predecessor:Nat
    def successor:Nat = new Succ(this)
    def + (that:Nat):Nat = {
      if (!that.isZero) successor + that.predecessor
      else this
    }
    def - (that:Nat):Nat = {
      if (isZero && !that.isZero) throw new IllegalStateException("bob")
      else if (!that.isZero) predecessor - that.predecessor
      else this
    }

  }

  object Zero extends Nat {
    def isZero = true
    def predecessor = throw new IllegalStateException("predecessor on zero")
    // alternative to complicated +,- is
    // def + (that:Nat):Nat = that
    // def - (that:Nat):Nat = if (that.isZero) this else throw new Error()
  }

  class Succ(n:Nat) extends Nat {
    def isZero = false
    override def predecessor: Nat = n
    //alternative to complicated +,- is
    // def + (that:Nat):Nat = new Succ(n+that)
    // def - (that:Nat):Nat = if (that.isZero) this else n - that predecessor
  }

  val zero:Nat = Zero
  val one:Nat = zero.successor
  val two:Nat = one.successor
  val three:Nat = two.successor
  val three2:Nat = one + two

  val result = three2 - three
  result.isZero

object b {
  trait aList[T] {
    def head: T
    def tail: aList[T]
    def isEmpty: Boolean
  }

  class Nil[T] extends aList[T] {
    def head = throw new java.util.NoSuchElementException("head of EmptyList")
    def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
    def isEmpty = true
  }

  class Cons[T](val head: T, val tail: aList[T]) extends aList[T] {
    def isEmpty = false
  }

  object aList {
    // aList(1,2)
    // this works because of the way functions get turned into objects

    def apply[T](x1:T, x2:T): aList[T] = new Cons(x1, new Cons(x2,new Nil))
    def apply[T]() = new Nil
   }


}

b.aList(2,3)

/*
class IntSet
class NonEmpty extends IntSet
object Empty extends IntSet
val a: Array[NonEmpty] = Array(new NonEmpty)
val z: Array[IntSet] = a
z(0) = Empty
val s: NonEmpty = a(0)
*/

trait Expr
case class Num(n:Int) extends Expr
case class Sum(e1:Expr, e2:Expr) extends Expr
case class Prod(e1:Expr, e2:Expr) extends Expr
case class Var(s:String) extends Expr


def show(e:Expr):String = {
  e match {
    case Num(n) => n.toString
    case Sum(e1, e2) => show(e1) + "+" + show(e2)
    case Prod(n:Sum,e2) => "("+show(n)+")"+"*"+show(e2)
    case Prod(l,r) => show(l) + "*"+show(r)
    case Var(s) => s
  }
}

val s: Num = Num(2)
val r: Num = Num(3)
val add:Expr = Sum(s,r)

show(s)
show(add)

show(Sum(Prod(Num(2),Var("x")), Var("y")))

show(Prod(Sum(Num(2),Var("x")),Sum(Num(3),Var("y"))))

def insert(x:Int,xs:List[Int]): List[Int] = xs match {
  case List() => x :: Nil
  case y :: ys => if (y>=x) x :: xs else y :: insert(x,ys)
}

