package recfun

object PascalSuiteWorksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(255); 
  def pascal(c: Int, r: Int): Int = {

    def calculate(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

    if (c > r || c < 0 || r < 0) 0 else calculate(c, r)
  };System.out.println("""pascal: (c: Int, r: Int)Int""");$skip(16); val res$0 = 

  pascal(0, 2);System.out.println("""res0: Int = """ + $show(res$0));$skip(15); val res$1 = 
  pascal(1, 2);System.out.println("""res1: Int = """ + $show(res$1));$skip(15); val res$2 = 
  pascal(1, 3);System.out.println("""res2: Int = """ + $show(res$2));$skip(15); val res$3 = 
  pascal(4, 5);System.out.println("""res3: Int = """ + $show(res$3));$skip(15); val res$4 = 
  pascal(5, 4);System.out.println("""res4: Int = """ + $show(res$4));$skip(16); val res$5 = 
  pascal(-1, 0);System.out.println("""res5: Int = """ + $show(res$5));$skip(627); 

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
  };System.out.println("""balance: (chars: List[Char])Boolean""");$skip(41); val res$6 = 
  
  balance("(just an) example".toList);System.out.println("""res6: Boolean = """ + $show(res$6));$skip(25); val res$7 = 
  balance("())(".toList);System.out.println("""res7: Boolean = """ + $show(res$7));$skip(21); val res$8 = 
  balance("".toList);System.out.println("""res8: Boolean = """ + $show(res$8));$skip(47); val res$9 = 
  balance("(if (zero? x) max (/ 1 x))".toList);System.out.println("""res9: Boolean = """ + $show(res$9));$skip(85); val res$10 = 
  balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList);System.out.println("""res10: Boolean = """ + $show(res$10));$skip(72); val res$11 = 
                                                  balance(":-)".toList);System.out.println("""res11: Boolean = """ + $show(res$11));$skip(461); 
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
  };System.out.println("""countChange: (money: Int, coins: List[Int])Int""");$skip(31); val res$12 = 
  
  countChange(4, List(1,2));System.out.println("""res12: Int = """ + $show(res$12));$skip(48); val res$13 = 
  countChange(300,List(5,10,20,50,100,200,500));System.out.println("""res13: Int = """ + $show(res$13));$skip(48); val res$14 = 
  countChange(301,List(5,10,20,50,100,200,500));System.out.println("""res14: Int = """ + $show(res$14));$skip(48); val res$15 = 
  countChange(300,List(500,5,50,100,20,200,10));System.out.println("""res15: Int = """ + $show(res$15))}
}
