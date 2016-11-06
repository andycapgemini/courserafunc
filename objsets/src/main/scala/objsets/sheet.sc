import java.util.NoSuchElementException

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  override def incl(x:Int):IntSet = new NonEmptySet(x,Empty,Empty)
  override def contains(x:Int):Boolean = false
  override def union(other:IntSet):IntSet = {
    println("union empty and "+other)
    other
  }
  override def toString():String = "."
}

class NonEmptySet(elem:Int, left:IntSet, right:IntSet) extends IntSet {

  override def contains(x: Int): Boolean =
    if (x < elem) left.contains(x)
    else if (elem < x) right.contains(x)
    else true

  override def incl(x: Int): IntSet = {
    if (x < elem) new NonEmptySet(elem, left.incl(x), right)
    else if (elem < x) new NonEmptySet(elem, left, right.incl(x))
    else this
  }

  override def union(other: IntSet):IntSet = {
    println(this + " union " + other)
    left union right union other incl elem
  }

  override def toString():String = "{" + left + elem + right + "}"

}

val one:IntSet = Empty.incl(1)
val five:IntSet = Empty.incl(5)
val three:IntSet = new NonEmptySet(3,one,five)

val two:IntSet = Empty.incl(2)
val six:IntSet = Empty.incl(6)
val four:IntSet = new NonEmptySet(4,two,six)

three union four

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head:T, val tail: List[T]) extends List[T] {
  def isEmpty = false
  //generated methods head and tail implement trait methods
  override def toString():String = head + "," + tail
}

class Nil[T] extends List[T] {
  def isEmpty:Boolean = true
  def head:Nothing = throw new NoSuchElementException("Nil head")
  def tail:Nothing = throw new NoSuchElementException("Nil tail")
  override def toString():String = "Nil"
}

def singleton[T](elem:T) = new Cons[T](elem, new Nil[T])

singleton[Int](1)
singleton(10) //type inference

object nth {
  def nth[T](index: Int, list: List[T]): T = {
    if (list.isEmpty) throw new IndexOutOfBoundsException("oops")
    if (index == 0)  list.head
    else nth(index - 1, list.tail)
  }
}

val l = new Cons(1, new Cons(2, new Cons(3, new Nil)))

nth.nth(2,l)
//nth.nth(-1, l)

val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

def compare(s:String):Boolean = {
  println(s)
  false
}
val bob:String = "ff gg"

bob.contains("gg")

google.exists(word => compare(word))
println(bob)
