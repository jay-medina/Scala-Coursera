package com.mrfunkycold.demo.week2.hw.funsets

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
  }


  test("singletonSet(1) contains 1") {
    new TestSets {
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains elements that are in both sets") {
    new TestSets {
      var one = union(union(s1,s2), s3)
      var two = union(union(s1,s3), s4)

      var result = intersect(one,two)

      assert(contains(result, 1), "1")
      assert(contains(result, 3), "3")
      assert(!contains(result,2), "2")
      assert(!contains(result,4), "4")
    }
  }

  test("differences contains elements that are in both sets") {
    new TestSets {
      val one = union(union(union(s1,s2), s3), s4)
      val two = union(union(s3,s4), s5)

      val result = diff(one,two)

      assert(contains(result, 1), "1")
      assert(contains(result, 2), "2")
      assert(!contains(result,3), "3")
      assert(!contains(result,4), "4")
      assert(!contains(result,5), "5")
    }
  }

  test("filter set of elements") {
    new TestSets {
      val one = union(union(union(s1,s2), s3), s4)

      val two = filter(one, union(s1,s2))

      assert(contains(two, 1), "1")
      assert(contains(two, 2), "2")
      assert(!contains(two,3), "3")
      assert(!contains(two,4), "4")
      assert(!contains(two,5), "5")
    }
  }

  test("for all bounded elements in s should be in p") {
    var s = union(singletonSet(2), singletonSet(4))
        s = union(s, union(singletonSet(6), singletonSet(10)))

    assert(forall(s, x => x % 2 == 0), "all elements are even")
    assert(forall(s, _ < 12), "all elements are less than 12")
  }

  test("there exists an elements in s should be in p") {
    var s = union(singletonSet(2), singletonSet(4))
    s = union(s, union(singletonSet(6), singletonSet(10)))

    assert(exists(s, _ == 4), "exist an element that is 4")
    assert(exists(s, _ > 8), "exist an element that is greater than 8")
  }

  test("map function transform set to new set") {
    var s = union(singletonSet(2), singletonSet(4))
    s = union(s, union(singletonSet(6), singletonSet(10)))

    val plusone = map(s, _ + 1)

    assert(forall(plusone, _ % 2 == 1), "all items are odd")
    assert(exists(plusone, _ == 7), "7 is in the new set")
  }


}
