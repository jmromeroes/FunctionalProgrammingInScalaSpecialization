package week1.recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }

    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(balance(":-)".toList))
    println(balance("())(".toList))

    println(countChange(4, List(1)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if(c < 0 || c > r) {
      0
    } else if(c == 0 || c == r){
      1
    } else {
      pascal(c-1, r-1) + pascal(c, r-1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def recursion(chars: List[Char], openCount: Int): Boolean ={
      if(chars.isEmpty) {
        if (openCount == 0) true else false
      } else if(openCount < 0){
        false
      } else {
        if(chars.head == '('){
          recursion(chars.tail, openCount+1);
        } else if(chars.head == ')'){
          recursion(chars.tail, openCount-1)
        } else {
          recursion(chars.tail, openCount)
        }
      }
    }

    recursion(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(coins.isEmpty) {
      0
    } else {
        if(money - coins.head < 0) {
          0
        } else if (money - coins.head == 0) {
          1
        } else {
          countChange(money-coins.head, coins) +
          countChange(money, coins.tail)
        }
    }
  }
}
