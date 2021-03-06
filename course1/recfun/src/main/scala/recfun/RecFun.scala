package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if ( {r == 0} || {c == 0} || {c == r }) 1
    else pascal(c - 1, r - 1) + pascal(c , r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def checkChar(openParenCount: Int, remChars: List[Char]): Boolean = {
      if (openParenCount == 0) {
        if (remChars.isEmpty) true
        else if (remChars.head == ')') false
        else if (remChars.head == '(') checkChar(openParenCount + 1, remChars.tail)
        else checkChar(openParenCount, remChars.tail)
      }
      else {
        if (remChars.isEmpty) false
        else if (remChars.head == ')') checkChar(openParenCount - 1, remChars.tail)
        else if (remChars.head == '(') checkChar(openParenCount + 1, remChars.tail)
        else checkChar(openParenCount, remChars.tail)
      }
    }
    checkChar(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0
    else if (money == 0) 1
    else if (money >= coins.head) countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else countChange(money, coins.tail)
  }
}
