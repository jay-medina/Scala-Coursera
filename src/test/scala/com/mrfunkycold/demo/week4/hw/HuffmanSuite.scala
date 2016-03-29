package com.mrfunkycold.demo.week4.hw

import com.mrfunkycold.demo.week4.hw.Huffman._
import org.scalatest.FunSuite

class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
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

  test("make code tree from two sub trees") {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a','b'), 5)
    val t2 = Fork(Leaf('c', 5), Leaf('d', 1), List('c','d'), 6)

    val result = makeCodeTree(t1, t2)

    assert(result === Fork(t1, t2, List('a','b','c','d'), 11))
  }

  test("number of times each char appears in a list") {
    val t1 = List('a','b','c','a','b','a','d');

    assert(times(t1) === List(('a',3), ('b', 2), ('c', 1), ('d', 1)))
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton") {
    val t1 = Leaf('e', 2)

    assert(singleton(List(t1)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine until there is a single tree node") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val expected = List(Fork( Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x', 4), List('e','t','x'), 7) )

    assert(until(singleton, combine)(leaflist) === expected)

  }
  test("make code tree from List of characters") {
    val chars = List('x', 'e','t','x', 't','x', 'x')
    val expected = Fork( Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x', 4), List('e','t','x'), 7)

    assert(createCodeTree(chars) === expected)
  }

  test("decode example in page") {
    val chars = List('a','a','a','a','a','a','a','a',
                     'b','b','b','c','d','e','f','g','h').map(_.toUpper)

    val codeTree = createCodeTree(chars)

    val decoded = decode(codeTree, "1000101010".toList.map(_.asDigit))

    assert(decoded.mkString === "AGBB")
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("code table for small table of characters") {
    val table = List( ('a', List(1,0,1,1)),
                      ('b', List(1,1,0,0) )
                    )
    assert(codeBits(table)('a') === List(1,0,1,1))
  }

  test("convert tree to table") {
    val tree = createCodeTree("aab".toList)
    val table = convert(tree)

    assert(codeBits(table)('a') === encode(tree)("a".toList))

  }

  test("decode and quickEncode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
