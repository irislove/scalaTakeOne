object patmatWorksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(68); 
  println("Welcome to the Scala worksheet")

  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree;$skip(352); 

  // Part 1: Basics

  def weight(tree: CodeTree): Int = tree match {
    case Fork(left, right, chars, weight) => weight
    case Leaf(char, weight) => weight
  };System.out.println("""weight: (tree: patmatWorksheet.CodeTree)Int""");$skip(64); 

  val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5);System.out.println("""t1  : patmatWorksheet.Fork = """ + $show(t1 ));$skip(107); 
  val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9);System.out.println("""t2  : patmatWorksheet.Fork = """ + $show(t2 ));$skip(14); val res$0 = 

  weight(t1);System.out.println("""res0: Int = """ + $show(res$0));$skip(14); val res$1 = 

  weight(t2);System.out.println("""res1: Int = """ + $show(res$1));$skip(153); 

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(left, right, chars, weight) => chars
    case Leaf(char, weight) => List(char)
  };System.out.println("""chars: (tree: patmatWorksheet.CodeTree)List[Char]""");$skip(13); val res$2 = 

  chars(t2);System.out.println("""res2: List[Char] = """ + $show(res$2));$skip(541); 

  def times(chars: List[Char]): List[(Char, Int)] = {
    def resultsPair(char: Char, charsList: List[Char], resultList: List[(Char, Int)]): List[(Char, Int)] = {
      val reminderList = charsList.filter(Char => char != Char)

      if (reminderList.isEmpty) (charsList.head, charsList.count(Char => char == Char)) :: resultList
      else resultsPair(reminderList.head, reminderList, (charsList.head, charsList.count(Char => char == Char)) :: resultList)
    }

    if (chars.isEmpty) Nil
    else resultsPair(chars.head, chars, Nil)
  };System.out.println("""times: (chars: List[Char])List[(Char, Int)]""");$skip(55); val res$3 = 

  times(List('a', 'b', 'a', 'b', 'c', 'd', 'd', 'e'));System.out.println("""res3: List[(Char, Int)] = """ + $show(res$3));$skip(13); val res$4 = 
  times(Nil);System.out.println("""res4: List[(Char, Int)] = """ + $show(res$4));$skip(909); 

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {

    def findMax(reminderList: List[(Char, Int)], currentPair: (Char, Int)): Leaf = {
      val greaterThanList = reminderList.filter((anotherPair: (Char, Int)) => anotherPair._2 > currentPair._2)

      if (greaterThanList.isEmpty) new Leaf(currentPair._1, currentPair._2)
      else findMax(greaterThanList, greaterThanList.head)
    }

    def constructList(reminderList: List[(Char, Int)], sortedList: List[Leaf]): List[Leaf] = {

      if (reminderList.isEmpty) Nil
      else if (reminderList.tail.isEmpty) new Leaf(reminderList.head._1, reminderList.head._2) :: sortedList
      else {
        val max = findMax(reminderList, reminderList.head)
        val restList = reminderList.filter((pair: (Char, Int)) => pair._1 != max.char)

        constructList(restList, max :: sortedList)
      }
    }

    constructList(freqs, Nil)
  };System.out.println("""makeOrderedLeafList: (freqs: List[(Char, Int)])List[patmatWorksheet.Leaf]""");$skip(89); val res$5 = 

  makeOrderedLeafList(List(('e', 1), ('d', 2), ('c', 1), ('b', 2), ('a', 2), ('f', 3)));System.out.println("""res5: List[patmatWorksheet.Leaf] = """ + $show(res$5));$skip(68); 
  def singleton(trees: List[CodeTree]): Boolean = trees.length == 1;System.out.println("""singleton: (trees: List[patmatWorksheet.CodeTree])Boolean""");$skip(64); val res$6 = 

  singleton(List(new Leaf('a', 2)) ::: List(new Leaf('b', 1)));System.out.println("""res6: Boolean = """ + $show(res$6));$skip(137); 

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right));System.out.println("""makeCodeTree: (left: patmatWorksheet.CodeTree, right: patmatWorksheet.CodeTree)patmatWorksheet.Fork""");$skip(403); 

  def combine(trees: List[CodeTree]): List[CodeTree] = {

    if (singleton(trees) || trees == Nil) trees
    else {
      val fork = makeCodeTree(trees.head, trees.tail.head)
      val frontList = trees.tail.tail.filter(CodeTree => weight(CodeTree) < fork.weight)
      val tailList = trees.tail.tail.filter(CodeTree => weight(CodeTree) >= fork.weight)

      frontList ::: fork :: tailList
    }
  };System.out.println("""combine: (trees: List[patmatWorksheet.CodeTree])List[patmatWorksheet.CodeTree]""");$skip(79); 

  val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4), Leaf('a', 5));System.out.println("""leaflist  : List[patmatWorksheet.Leaf] = """ + $show(leaflist ));$skip(20); val res$7 = 
  combine(leaflist);System.out.println("""res7: List[patmatWorksheet.CodeTree] = """ + $show(res$7));$skip(29); val res$8 = 
  combine(combine(leaflist));System.out.println("""res8: List[patmatWorksheet.CodeTree] = """ + $show(res$8));$skip(38); val res$9 = 
  combine(combine(combine(leaflist)));System.out.println("""res9: List[patmatWorksheet.CodeTree] = """ + $show(res$9));$skip(32); val res$10 = 

  singleton(combine(leaflist));System.out.println("""res10: Boolean = """ + $show(res$10));$skip(41); val res$11 = 

  singleton(combine(combine(leaflist)));System.out.println("""res11: Boolean = """ + $show(res$11));$skip(49); val res$12 = 
  singleton(combine(combine(combine(leaflist))));System.out.println("""res12: Boolean = """ + $show(res$12));$skip(305); 

  def until(predicate: List[CodeTree] => Boolean, f: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    def createTree(newTrees: List[CodeTree]): List[CodeTree] = {
      if (!predicate(newTrees)) createTree(f(newTrees))
      else newTrees
    }

    createTree(trees)
  };System.out.println("""until: (predicate: List[patmatWorksheet.CodeTree] => Boolean, f: List[patmatWorksheet.CodeTree] => List[patmatWorksheet.CodeTree])(trees: List[patmatWorksheet.CodeTree])List[patmatWorksheet.CodeTree]""");$skip(39); val res$13 = 

  until(singleton, combine)(leaflist);System.out.println("""res13: List[patmatWorksheet.CodeTree] = """ + $show(res$13));$skip(129); 

  def createCodeTree(chars: List[Char]): CodeTree = {
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head
  };System.out.println("""createCodeTree: (chars: List[Char])patmatWorksheet.CodeTree""");$skip(84); val res$14 = 

  createCodeTree(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  type Bit = Int;System.out.println("""res14: patmatWorksheet.CodeTree = """ + $show(res$14));$skip(228); 

  def leftTree(tree: CodeTree): CodeTree = tree match {
    case Fork(left, right, chars, weight) => left
    case Leaf(char, weight) => throw new NoSuchElementException("Leaf node doesn't have left tree")
  };System.out.println("""leftTree: (tree: patmatWorksheet.CodeTree)patmatWorksheet.CodeTree""");$skip(214); 

  def rightTree(tree: CodeTree): CodeTree = tree match {
    case Fork(left, right, chars, weight) => right
    case Leaf(char, weight) => throw new NoSuchElementException("Leaf node doesn't have right tree")
  };System.out.println("""rightTree: (tree: patmatWorksheet.CodeTree)patmatWorksheet.CodeTree""");$skip(145); 

  def isLeaf(tree: CodeTree): Boolean = tree match {
    case Fork(left, right, chars, weight) => false
    case Leaf(char, weight) => true
  };System.out.println("""isLeaf: (tree: patmatWorksheet.CodeTree)Boolean""");$skip(930); 
  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def decodeBits(reminderTree: CodeTree, reminderBits: List[Bit], charList: List[Char]): List[Char] = {
      if (reminderBits.isEmpty) charList
      else if (reminderBits.head == 0 && !isLeaf(leftTree(reminderTree))) decodeBits(leftTree(reminderTree), reminderBits.tail, charList)
      else if (reminderBits.head == 1 && !isLeaf(rightTree(reminderTree))) decodeBits(rightTree(reminderTree), reminderBits.tail, charList)
      else if (reminderBits.head == 0) decodeBits(tree, reminderBits.tail, charList ::: chars(leftTree(reminderTree)))
      else if (reminderBits.head == 1) decodeBits(tree, reminderBits.tail, charList ::: chars(rightTree(reminderTree)))
      else charList
    }

    decodeBits(tree, bits, Nil)
  };System.out.println("""decode: (tree: patmatWorksheet.CodeTree, bits: List[patmatWorksheet.Bit])List[Char]""");$skip(1718); 
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387);System.out.println("""frenchCode  : patmatWorksheet.CodeTree = """ + $show(frenchCode ));$skip(220); 
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1);System.out.println("""secret  : List[patmatWorksheet.Bit] = """ + $show(secret ));$skip(20); val res$15 = 
  chars(frenchCode);System.out.println("""res15: List[Char] = """ + $show(res$15));$skip(61); 
  def decodedSecret: List[Char] = decode(frenchCode, secret);System.out.println("""decodedSecret: => List[Char]""");$skip(17); val res$16 = 

  decodedSecret;System.out.println("""res16: List[Char] = """ + $show(res$16));$skip(702); 

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def encodeTree(reminderTree: CodeTree, reminderText: List[Char], encodedBits: List[Bit]): List[Bit] = {
      if (reminderText.isEmpty) encodedBits
      else if (isLeaf(reminderTree)) encodeTree(tree, reminderText.tail, encodedBits)
      else if (chars(leftTree(reminderTree)).contains(reminderText.head)) encodeTree(leftTree(reminderTree), reminderText, encodedBits ::: List(0))
      else if (chars(rightTree(reminderTree)).contains(reminderText.head)) encodeTree(rightTree(reminderTree), reminderText, encodedBits ::: List(1))
      else encodeTree(tree, reminderText.tail, encodedBits)
    }

    encodeTree(tree, text, Nil)
  };System.out.println("""encode: (tree: patmatWorksheet.CodeTree)(text: List[Char])List[patmatWorksheet.Bit]""");$skip(42); val res$17 = 
  encode(frenchCode)(List('s', 'd', 'x'));System.out.println("""res17: List[patmatWorksheet.Bit] = """ + $show(res$17));$skip(70); val res$18 = 

  decode(frenchCode, List(0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0));System.out.println("""res18: List[Char] = """ + $show(res$18));$skip(29); val res$19 = 
  encode(t1)(List('a', 'b'));System.out.println("""res19: List[patmatWorksheet.Bit] = """ + $show(res$19));$skip(25); val res$20 = 
  decode(t1, List(0, 1));System.out.println("""res20: List[Char] = """ + $show(res$20));$skip(96); 
  val frenchSecret = List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l');System.out.println("""frenchSecret  : List[Char] = """ + $show(frenchSecret ));$skip(61); 
  val bitsList: List[Bit] = encode(frenchCode)(frenchSecret);System.out.println("""bitsList  : List[patmatWorksheet.Bit] = """ + $show(bitsList ));$skip(32); val res$21 = 

  decode(frenchCode, bitsList)

  type CodeTable = List[(Char, List[Bit])];System.out.println("""res21: List[Char] = """ + $show(res$21));$skip(305); 

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
    table.filter((pair: (Char, List[Bit])) => pair._1 == char).head._2
  };System.out.println("""codeBits: (table: patmatWorksheet.CodeTable)(char: Char)List[patmatWorksheet.Bit]""");$skip(317); 

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    a ::: b
  };System.out.println("""mergeCodeTables: (a: patmatWorksheet.CodeTable, b: patmatWorksheet.CodeTable)patmatWorksheet.CodeTable""");$skip(1518); 

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = {
    def addBitToCodeTable(codeTable: CodeTable, newCodeTable: CodeTable, bit: Bit): CodeTable = {
      if (codeTable.tail.isEmpty) newCodeTable ::: List((codeTable.head._1, List(bit) ::: codeTable.head._2))
      else addBitToCodeTable(codeTable.tail, newCodeTable ::: List((codeTable.head._1, List(bit) ::: codeTable.head._2)), bit)
    }

    if (isLeaf(leftTree(tree)) && isLeaf(rightTree(tree))) {
      mergeCodeTables(List((chars(leftTree(tree)).head, List(0))), List((chars(rightTree(tree)).head, List(1))))
    } else if (!isLeaf(leftTree(tree)) && isLeaf(rightTree(tree))) {
      mergeCodeTables(addBitToCodeTable(convert(leftTree(tree)), Nil, 0), List((chars(rightTree(tree)).head, List(1))))
    } else if (isLeaf(leftTree(tree)) && !isLeaf(rightTree(tree))) {
      mergeCodeTables(List((chars(leftTree(tree)).head, List(0))), addBitToCodeTable(convert(rightTree(tree)), Nil, 1))
    } else {
      mergeCodeTables(addBitToCodeTable(convert(leftTree(tree)), Nil, 0), addBitToCodeTable(convert(rightTree(tree)), Nil, 1))
    }
  };System.out.println("""convert: (tree: patmatWorksheet.CodeTree)patmatWorksheet.CodeTable""");$skip(15); val res$22 = 

  convert(t1);System.out.println("""res22: patmatWorksheet.CodeTable = """ + $show(res$22));$skip(14); val res$23 = 
  convert(t2);System.out.println("""res23: patmatWorksheet.CodeTable = """ + $show(res$23));$skip(25); val res$24 = 

  encode(t2)(List('d'));System.out.println("""res24: List[patmatWorksheet.Bit] = """ + $show(res$24));$skip(24); val res$25 = 
  encode(t2)(List('b'));System.out.println("""res25: List[patmatWorksheet.Bit] = """ + $show(res$25));$skip(23); val res$26 = 

  convert(frenchCode);System.out.println("""res26: patmatWorksheet.CodeTable = """ + $show(res$26));$skip(32); val res$27 = 
  encode(frenchCode)(List('i'));System.out.println("""res27: List[patmatWorksheet.Bit] = """ + $show(res$27));$skip(33); val res$28 = 

  encode(frenchCode)(List('a'));System.out.println("""res28: List[patmatWorksheet.Bit] = """ + $show(res$28));$skip(33); val res$29 = 

  encode(frenchCode)(List('c'));System.out.println("""res29: List[patmatWorksheet.Bit] = """ + $show(res$29));$skip(381); 

  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val codeTable = convert(tree)

    def encode(textList: List[Char], encodedBits: List[Bit]): List[Bit] = {
      if (textList.tail.isEmpty) encodedBits ::: codeBits(codeTable)(textList.head)
      else encode(textList.tail, encodedBits ::: codeBits(codeTable)(textList.head))
    }

    encode(text, Nil)
  };System.out.println("""quickEncode: (tree: patmatWorksheet.CodeTree)(text: List[Char])List[patmatWorksheet.Bit]""");$skip(66); 
  val frenchSecretEncoded = quickEncode(frenchCode)(frenchSecret);System.out.println("""frenchSecretEncoded  : List[patmatWorksheet.Bit] = """ + $show(frenchSecretEncoded ));$skip(42); val res$30 = 
  decode(frenchCode, frenchSecretEncoded);System.out.println("""res30: List[Char] = """ + $show(res$30));$skip(30); val res$31 = 
  
  makeOrderedLeafList(Nil);System.out.println("""res31: List[patmatWorksheet.Leaf] = """ + $show(res$31));$skip(18); val res$32 = 
  
  combine(Nil);System.out.println("""res32: List[patmatWorksheet.CodeTree] = """ + $show(res$32))}
}
