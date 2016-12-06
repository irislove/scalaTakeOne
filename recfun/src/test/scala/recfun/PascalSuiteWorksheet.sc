package recfun

object PascalSuiteWorksheet {
  def pascal(c: Int, r: Int): Int = {

    def calculate(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

    if (c > r || c < 0 || r < 0) 0 else calculate(c, r)
  }                                               //> pascal: (c: Int, r: Int)Int

  pascal(0, 2)                                    //> res0: Int = 1
  pascal(1, 2)                                    //> res1: Int = 2
  pascal(1, 3)                                    //> res2: Int = 3
  pascal(4, 5)                                    //> res3: Int = 5
  pascal(5, 4)                                    //> res4: Int = 0
  pascal(-1, 0)                                   //> res5: Int = 0

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
  }                                               //> balance: (chars: List[Char])Boolean
  
  balance("(just an) example".toList)             //> res6: Boolean = true
  balance("())(".toList)                          //> res7: Boolean = false
  balance("".toList)                              //> res8: Boolean = false
  balance("(if (zero? x) max (/ 1 x))".toList)    //> res9: Boolean = true
  balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList)
                                                  //> res10: Boolean = true
                                                  balance(":-)".toList)
                                                  //> res11: Boolean = false
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
  }                                               //> countChange: (money: Int, coins: List[Int])Int
  
  countChange(4, List(1,2))                       //> res12: Int = 3
  countChange(300,List(5,10,20,50,100,200,500))   //> res13: Int = 1022
  countChange(301,List(5,10,20,50,100,200,500))   //> res14: Int = 0
  countChange(300,List(500,5,50,100,20,200,10))   //> res15: Int = 1022
}