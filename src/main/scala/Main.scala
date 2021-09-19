package recfun
//import common._

object Main {
  def main(args: Array[String]) {
//    println("Pascal's Triangle")
//    for (row <- 0 to 10) {
//      for (col <- 0 to row)
//        print(pascal(col, row) + " ")
//      println()
//    }

//    val countChangeRes: Int = countChange(4, List(1,2), "start")
//    println(countChangeRes)

    val pascalRes = pascal(2,5)
    println(pascalRes)
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (r < c) 0
    else if (c == r || c == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(chars: List[Char], counter: Int): Boolean = {
      if (counter < 0) false
      else
        if (chars.isEmpty) true
        else {
          val c = chars.head
          balanceIter(chars.tail, if (c.equals('(')) counter + 1 else if (c.equals(')')) counter - 1 else counter)
        }
    }
    balanceIter(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int], i: String): Int = {
    println("side " + i + " money " + money + " coins " + coins)
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money, coins.tail, "left") + countChange(money - coins.head, coins, "right")
  }
}