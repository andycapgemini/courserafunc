package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val leaf = Leaf('a',2)
    val hw = List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')
    val hwLeaves = List(('h',1),('e',1),('l',3),('o',2),(',',1),(' ',1),('w',1),('r',1),('d',1))
    val hwOrderedLeafList:List[Leaf] = List(Leaf('d',1),Leaf('r',1),Leaf('w',1),Leaf(' ',1),Leaf(',',1),Leaf('e',1),Leaf('h',1),Leaf('o',2),Leaf('l',3))
    val hwtree =
      Fork(
      Fork(Leaf('o',2),Fork(Leaf('h',1),Fork(Leaf(',',1),Leaf('e',1),List(',','e'),2),List('h',',','e'),3),List('o','h',',','e'),5),
        Fork(Leaf('l',3),Fork(Fork(Leaf('w',1),Leaf(' ',1),List('w',' '),2),Fork(Leaf('d',1),Leaf('r',1),List('d','r'),2),List('d','r','w',' '),4),List('l','d','r','w',' '),7),
        List('l','d','r','w',' ','o','h',',','e'),
        12)
  }




  test("weight leaf") {
    new TestTrees {
      assert(weight(leaf) === 2)
    }
  }

  test("chars leaf") {
    new TestTrees {
      assert(chars(leaf) === List('a'))
    }
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("hellow world tests") {
    new TestTrees {
      assert(times(hw) === hwLeaves)
      assert(makeOrderedLeafList(hwLeaves) === hwOrderedLeafList)
      assert(createCodeTree(hw).toString === hwtree.toString)
      assert(decode(hwtree,encode(hwtree)(hw)) === hw)
      assert(decode(hwtree,quickEncode(hwtree)(hw)) === hw)
    }
  }

  test("singleton") {
    new TestTrees {
      assert(singleton(List()) === false)
      assert(singleton(List(t1)) === true)
      assert(singleton(List(t1, t2)) === false)
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
