package com.varokas.kata.fizzbuzz

import org.scalatest.{Matchers, FunSuite}
import org.scalatest.matchers.ShouldMatchers


class FizzBuzzerTest extends FunSuite with Matchers {
  val fizzBuzzer = new FizzBuzzer()

  test("Zero should throw exception") {
    intercept[IllegalArgumentException] {
      fizzBuzzer.get(0)
    }
  }

  test("Negative number should throw exception") {
    intercept[IllegalArgumentException] {
      fizzBuzzer.get(-1)
    }
  }

  test("A positive Number is fizz if it is divisible by 3 (but not by 5)") {
    List(3,6,9,12).foreach { n =>
      fizzBuzzer.get(n) shouldBe "fizz"
    }
  }

  test("A positive Number is buzz if it is divisible by 5 (but not by 3)") {
    List(5,10,20).foreach { n =>
      fizzBuzzer.get(n) shouldBe "buzz"
    }
  }

  test("A positive Number is fizzbuzz if it is divisible by 3 and 5") {
    List(15,30,45,75).foreach { n =>
      fizzBuzzer.get(n) shouldBe "fizzbuzz"
    }
  }

  test("Other numbers returns itself") {
    List(1,2,4,7).foreach { n =>
      fizzBuzzer.get(n) shouldBe n.toString
    }
  }

  test("Generate should get a list of fizzbuzz") {
    fizzBuzzer.generate(15) shouldEqual List(
      "1","2","fizz","4","buzz",
      "fizz","7","8","fizz","buzz",
      "11","fizz","13","14","fizzbuzz")
  }
}
