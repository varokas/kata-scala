package com.varokas.kata.fizzbuzz

class FizzBuzzer {
  def get(n: Int):String = {
    require( n > 0 , "Only accepts positive integer" )

    if  (n % 3 == 0 && n % 5 == 0) "fizzbuzz"
    else if  (n % 3 == 0) "fizz"
    else if  (n % 5 == 0) "buzz"
    else n.toString
  }

  def generate(n: Int):Iterable[String] = {
    require( n >= 1, "n must be more than zero")

    (1 to n).map( i => get(i) )
  }
}
