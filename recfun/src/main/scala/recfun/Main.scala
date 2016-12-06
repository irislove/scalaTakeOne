package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    
    def calculate(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
      
    if (c > r || c < 0 || r < 0) 0 else calculate(c, r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    
    def isBalanced(count: Int, newChars: List[Char]): Boolean = {
      if (newChars.isEmpty) {
        count == 0
      } else if (newChars.head == "(".charAt(0)) {
        isBalanced(count + 1, newChars.tail) 
      } else if (newChars.head == ")".charAt(0)) {
        isBalanced(count - 1, newChars.tail) 
      } else {
        isBalanced(count, newChars.tail)
      }
    }
    
    if (chars.isEmpty || chars.indexOf("(".charAt(0)) > chars.indexOf(")".charAt(0)) || chars.lastIndexOf("(".charAt(0)) > chars.lastIndexOf(")".charAt(0))) false else isBalanced(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(remaining: Int, numCoins: Int, remainingCoins: List[Int]): Int = {
      if (remaining == 0) {
        1
      } else if (remaining < 0 || numCoins == 0 || remainingCoins.isEmpty) {
        0
      } else {
        count(remaining, numCoins - 1, remainingCoins.tail) + count(remaining - remainingCoins.head, numCoins, remainingCoins)
      }
    }
    count(money, coins.length, coins)
  }
}
