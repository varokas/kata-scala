package com.varokas.kata.collections

import org.scalatest.{Matchers, FunSuite}

class StreamTest extends FunSuite with Matchers {
  test("generate fibonacci") {
    def fibSeries(last: Long, current: Long):Stream[Long] = Stream.cons(last, fibSeries(current, last+current))
    def fibGen:Stream[Long] = fibSeries(1,1)

    fibGen.take(7) shouldBe List(1,1,2,3,5,8,13)
  }

  def isPrime(number:Int):Boolean = notFound( (2 until number).find( i => canDivide(number, i) ) )
  def notFound(option:Option[Int]):Boolean = option.isEmpty

  def canDivide(number: Int, i: Int): Boolean = (number % i == 0)

  test("isPrime(n)") {
    isPrime(2) shouldBe true
    isPrime(4) shouldBe false
    isPrime(6) shouldBe false
  }

  test("filter") {
    (2 until 10).filter( i => i % 2 == 0 ) shouldBe List(2,4,6,8)
  }
}
