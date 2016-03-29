package com.mrfunkycold.demo.demo

import org.junit.Assert._
import org.junit.{Before, Test}


class DemoTests{

  var demo: Demo = _

  @Before
  def setUp {
    demo = new Demo
  }

  @Test
  def testAddTwoNumbers {
    assertEquals(4, demo.addTwoNumbers(1,3))
  }
}
