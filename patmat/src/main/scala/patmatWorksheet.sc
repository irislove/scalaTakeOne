object patmatWorksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  // Part 1: Basics

  def weight(tree: CodeTree): Int = tree match {
    case Fork(left, right, chars, weight) => weight
    case Leaf(char, weight) => weight
  }                                               //> weight: (tree: patmatWorksheet.CodeTree)Int

  val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
                                                  //> t1  : patmatWorksheet.Fork = Fork(Leaf(a,2),Leaf(b,3),List(a, b),5)
  val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
                                                  //> t2  : patmatWorksheet.Fork = Fork(Fork(Leaf(a,2),Leaf(b,3),List(a, b),5),Lea
                                                  //| f(d,4),List(a, b, d),9)

  weight(t1)                                      //> res0: Int = 5

  weight(t2)                                      //> res1: Int = 9

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(left, right, chars, weight) => chars
    case Leaf(char, weight) => List(char)
  }                                               //> chars: (tree: patmatWorksheet.CodeTree)List[Char]

  chars(t2)                                       //> res2: List[Char] = List(a, b, d)

  def times(chars: List[Char]): List[(Char, Int)] = {
    def resultsPair(char: Char, charsList: List[Char], resultList: List[(Char, Int)]): List[(Char, Int)] = {
      val reminderList = charsList.filter(Char => char != Char)

      if (reminderList.isEmpty) (charsList.head, charsList.count(Char => char == Char)) :: resultList
      else resultsPair(reminderList.head, reminderList, (charsList.head, charsList.count(Char => char == Char)) :: resultList)
    }

    if (chars.isEmpty) Nil
    else resultsPair(chars.head, chars, Nil)
  }                                               //> times: (chars: List[Char])List[(Char, Int)]

  times(List('a', 'b', 'a', 'b', 'c', 'd', 'd', 'e'))
                                                  //> res3: List[(Char, Int)] = List((e,1), (d,2), (c,1), (b,2), (a,2))
  times(Nil)                                      //> res4: List[(Char, Int)] = List()

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
  }                                               //> makeOrderedLeafList: (freqs: List[(Char, Int)])List[patmatWorksheet.Leaf]

  makeOrderedLeafList(List(('e', 1), ('d', 2), ('c', 1), ('b', 2), ('a', 2), ('f', 3)))
                                                  //> res5: List[patmatWorksheet.Leaf] = List(Leaf(c,1), Leaf(e,1), Leaf(a,2), Le
                                                  //| af(b,2), Leaf(d,2), Leaf(f,3))
  def singleton(trees: List[CodeTree]): Boolean = trees.length == 1
                                                  //> singleton: (trees: List[patmatWorksheet.CodeTree])Boolean

  singleton(List(new Leaf('a', 2)) ::: List(new Leaf('b', 1)))
                                                  //> res6: Boolean = false

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))
                                                  //> makeCodeTree: (left: patmatWorksheet.CodeTree, right: patmatWorksheet.CodeT
                                                  //| ree)patmatWorksheet.Fork

  def combine(trees: List[CodeTree]): List[CodeTree] = {

    if (singleton(trees) || trees == Nil) trees
    else {
      val fork = makeCodeTree(trees.head, trees.tail.head)
      val frontList = trees.tail.tail.filter(CodeTree => weight(CodeTree) < fork.weight)
      val tailList = trees.tail.tail.filter(CodeTree => weight(CodeTree) >= fork.weight)

      frontList ::: fork :: tailList
    }
  }                                               //> combine: (trees: List[patmatWorksheet.CodeTree])List[patmatWorksheet.CodeTr
                                                  //| ee]

  val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4), Leaf('a', 5))
                                                  //> leaflist  : List[patmatWorksheet.Leaf] = List(Leaf(e,1), Leaf(t,2), Leaf(x,
                                                  //| 4), Leaf(a,5))
  combine(leaflist)                               //> res7: List[patmatWorksheet.CodeTree] = List(Fork(Leaf(e,1),Leaf(t,2),List(e
                                                  //| , t),3), Leaf(x,4), Leaf(a,5))
  combine(combine(leaflist))                      //> res8: List[patmatWorksheet.CodeTree] = List(Leaf(a,5), Fork(Fork(Leaf(e,1),
                                                  //| Leaf(t,2),List(e, t),3),Leaf(x,4),List(e, t, x),7))
  combine(combine(combine(leaflist)))             //> res9: List[patmatWorksheet.CodeTree] = List(Fork(Leaf(a,5),Fork(Fork(Leaf(e
                                                  //| ,1),Leaf(t,2),List(e, t),3),Leaf(x,4),List(e, t, x),7),List(a, e, t, x),12)
                                                  //| )

  singleton(combine(leaflist))                    //> res10: Boolean = false

  singleton(combine(combine(leaflist)))           //> res11: Boolean = false
  singleton(combine(combine(combine(leaflist))))  //> res12: Boolean = true

  def until(predicate: List[CodeTree] => Boolean, f: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    def createTree(newTrees: List[CodeTree]): List[CodeTree] = {
      if (!predicate(newTrees)) createTree(f(newTrees))
      else newTrees
    }

    createTree(trees)
  }                                               //> until: (predicate: List[patmatWorksheet.CodeTree] => Boolean, f: List[patma
                                                  //| tWorksheet.CodeTree] => List[patmatWorksheet.CodeTree])(trees: List[patmatW
                                                  //| orksheet.CodeTree])List[patmatWorksheet.CodeTree]

  until(singleton, combine)(leaflist)             //> res13: List[patmatWorksheet.CodeTree] = List(Fork(Leaf(a,5),Fork(Fork(Leaf(
                                                  //| e,1),Leaf(t,2),List(e, t),3),Leaf(x,4),List(e, t, x),7),List(a, e, t, x),12
                                                  //| ))

  def createCodeTree(chars: List[Char]): CodeTree = {
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head
  }                                               //> createCodeTree: (chars: List[Char])patmatWorksheet.CodeTree

  createCodeTree(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
                                                  //> res14: patmatWorksheet.CodeTree = Fork(Fork(Leaf(o,2),Fork(Leaf(d,1),Fork(L
                                                  //| eaf(w,1),Leaf(r,1),List(w, r),2),List(d, w, r),3),List(o, d, w, r),5),Fork(
                                                  //| Leaf(l,3),Fork(Fork(Leaf(,,1),Leaf( ,1),List(,,  ),2),Fork(Leaf(h,1),Leaf(e
                                                  //| ,1),List(h, e),2),List(,,  , h, e),4),List(l, ,,  , h, e),7),List(o, d, w, 
                                                  //| r, l, ,,  , h, e),12)
  type Bit = Int

  def leftTree(tree: CodeTree): CodeTree = tree match {
    case Fork(left, right, chars, weight) => left
    case Leaf(char, weight) => throw new NoSuchElementException("Leaf node doesn't have left tree")
  }                                               //> leftTree: (tree: patmatWorksheet.CodeTree)patmatWorksheet.CodeTree

  def rightTree(tree: CodeTree): CodeTree = tree match {
    case Fork(left, right, chars, weight) => right
    case Leaf(char, weight) => throw new NoSuchElementException("Leaf node doesn't have right tree")
  }                                               //> rightTree: (tree: patmatWorksheet.CodeTree)patmatWorksheet.CodeTree

  def isLeaf(tree: CodeTree): Boolean = tree match {
    case Fork(left, right, chars, weight) => false
    case Leaf(char, weight) => true
  }                                               //> isLeaf: (tree: patmatWorksheet.CodeTree)Boolean
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
  }                                               //> decode: (tree: patmatWorksheet.CodeTree, bits: List[patmatWorksheet.Bit])Li
                                                  //| st[Char]
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)
                                                  //> frenchCode  : patmatWorksheet.CodeTree = Fork(Fork(Fork(Leaf(s,121895),Fork
                                                  //| (Leaf(d,56269),Fork(Fork(Fork(Leaf(x,5928),Leaf(j,8351),List(x, j),14279),L
                                                  //| eaf(f,16351),List(x, j, f),30630),Fork(Fork(Fork(Fork(Leaf(z,2093),Fork(Lea
                                                  //| f(k,745),Leaf(w,1747),List(k, w),2492),List(z, k, w),4585),Leaf(y,4725),Lis
                                                  //| t(z, k, w, y),9310),Leaf(h,11298),List(z, k, w, y, h),20608),Leaf(q,20889),
                                                  //| List(z, k, w, y, h, q),41497),List(x, j, f, z, k, w, y, h, q),72127),List(d
                                                  //| , x, j, f, z, k, w, y, h, q),128396),List(s, d, x, j, f, z, k, w, y, h, q),
                                                  //| 250291),Fork(Fork(Leaf(o,82762),Leaf(l,83668),List(o, l),166430),Fork(Fork(
                                                  //| Leaf(m,45521),Leaf(p,46335),List(m, p),91856),Leaf(u,96785),List(m, p, u),1
                                                  //| 88641),List(o, l, m, p, u),355071),List(s, d, x, j, f, z, k, w, y, h, q, o,
                                                  //|  l, m, p, u),605362),Fork(Fork(Fork(Leaf(r,100500),Fork(Leaf(c,50003),Fork(
                                                  //| Leaf(v,24975),Fork(Leaf(g,13288),Leaf(b,13822),List(g, b),27110),List(v, g,
                                                  //|  b),52085),List(c, v, g
                                                  //| Output exceeds cutoff limit.
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)
                                                  //> secret  : List[patmatWorksheet.Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1,
                                                  //|  0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0,
                                                  //|  1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1,
                                                  //|  0, 1)
  chars(frenchCode)                               //> res15: List[Char] = List(s, d, x, j, f, z, k, w, y, h, q, o, l, m, p, u, r,
                                                  //|  c, v, g, b, n, t, e, i, a)
  def decodedSecret: List[Char] = decode(frenchCode, secret)
                                                  //> decodedSecret: => List[Char]

  decodedSecret                                   //> res16: List[Char] = List(h, u, f, f, m, a, n, e, s, t, c, o, o, l)

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def encodeTree(reminderTree: CodeTree, reminderText: List[Char], encodedBits: List[Bit]): List[Bit] = {
      if (reminderText.isEmpty) encodedBits
      else if (isLeaf(reminderTree)) encodeTree(tree, reminderText.tail, encodedBits)
      else if (chars(leftTree(reminderTree)).contains(reminderText.head)) encodeTree(leftTree(reminderTree), reminderText, encodedBits ::: List(0))
      else if (chars(rightTree(reminderTree)).contains(reminderText.head)) encodeTree(rightTree(reminderTree), reminderText, encodedBits ::: List(1))
      else encodeTree(tree, reminderText.tail, encodedBits)
    }

    encodeTree(tree, text, Nil)
  }                                               //> encode: (tree: patmatWorksheet.CodeTree)(text: List[Char])List[patmatWorksh
                                                  //| eet.Bit]
  encode(frenchCode)(List('s', 'd', 'x'))         //> res17: List[patmatWorksheet.Bit] = List(0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0,
                                                  //|  0, 0)

  decode(frenchCode, List(0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0))
                                                  //> res18: List[Char] = List(s, d, x)
  encode(t1)(List('a', 'b'))                      //> res19: List[patmatWorksheet.Bit] = List(0, 1)
  decode(t1, List(0, 1))                          //> res20: List[Char] = List(a, b)
  val frenchSecret = List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')
                                                  //> frenchSecret  : List[Char] = List(h, u, f, f, m, a, n, e, s, t, c, o, o, l)
                                                  //| 
  val bitsList: List[Bit] = encode(frenchCode)(frenchSecret)
                                                  //> bitsList  : List[patmatWorksheet.Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 
                                                  //| 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 
                                                  //| 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 
                                                  //| 1, 0, 1)

  decode(frenchCode, bitsList)                    //> res21: List[Char] = List(h, u, f, f, m, a, n, e, s, t, c, o, o, l)

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
    table.filter((pair: (Char, List[Bit])) => pair._1 == char).head._2
  }                                               //> codeBits: (table: patmatWorksheet.CodeTable)(char: Char)List[patmatWorkshee
                                                  //| t.Bit]

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    a ::: b
  }                                               //> mergeCodeTables: (a: patmatWorksheet.CodeTable, b: patmatWorksheet.CodeTabl
                                                  //| e)patmatWorksheet.CodeTable

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
  }                                               //> convert: (tree: patmatWorksheet.CodeTree)patmatWorksheet.CodeTable

  convert(t1)                                     //> res22: patmatWorksheet.CodeTable = List((a,List(0)), (b,List(1)))
  convert(t2)                                     //> res23: patmatWorksheet.CodeTable = List((a,List(0, 0)), (b,List(0, 1)), (d
                                                  //| ,List(1)))

  encode(t2)(List('d'))                           //> res24: List[patmatWorksheet.Bit] = List(1)
  encode(t2)(List('b'))                           //> res25: List[patmatWorksheet.Bit] = List(0, 1)

  convert(frenchCode)                             //> res26: patmatWorksheet.CodeTable = List((s,List(0, 0, 0)), (d,List(0, 0, 1
                                                  //| , 0)), (x,List(0, 0, 1, 1, 0, 0, 0)), (j,List(0, 0, 1, 1, 0, 0, 1)), (f,Li
                                                  //| st(0, 0, 1, 1, 0, 1)), (z,List(0, 0, 1, 1, 1, 0, 0, 0, 0)), (k,List(0, 0, 
                                                  //| 1, 1, 1, 0, 0, 0, 1, 0)), (w,List(0, 0, 1, 1, 1, 0, 0, 0, 1, 1)), (y,List(
                                                  //| 0, 0, 1, 1, 1, 0, 0, 1)), (h,List(0, 0, 1, 1, 1, 0, 1)), (q,List(0, 0, 1, 
                                                  //| 1, 1, 1)), (o,List(0, 1, 0, 0)), (l,List(0, 1, 0, 1)), (m,List(0, 1, 1, 0,
                                                  //|  0)), (p,List(0, 1, 1, 0, 1)), (u,List(0, 1, 1, 1)), (r,List(1, 0, 0, 0)),
                                                  //|  (c,List(1, 0, 0, 1, 0)), (v,List(1, 0, 0, 1, 1, 0)), (g,List(1, 0, 0, 1, 
                                                  //| 1, 1, 0)), (b,List(1, 0, 0, 1, 1, 1, 1)), (n,List(1, 0, 1, 0)), (t,List(1,
                                                  //|  0, 1, 1)), (e,List(1, 1, 0)), (i,List(1, 1, 1, 0)), (a,List(1, 1, 1, 1)))
                                                  //| 
  encode(frenchCode)(List('i'))                   //> res27: List[patmatWorksheet.Bit] = List(1, 1, 1, 0)

  encode(frenchCode)(List('a'))                   //> res28: List[patmatWorksheet.Bit] = List(1, 1, 1, 1)

  encode(frenchCode)(List('c'))                   //> res29: List[patmatWorksheet.Bit] = List(1, 0, 0, 1, 0)

  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val codeTable = convert(tree)

    def encode(textList: List[Char], encodedBits: List[Bit]): List[Bit] = {
      if (textList.tail.isEmpty) encodedBits ::: codeBits(codeTable)(textList.head)
      else encode(textList.tail, encodedBits ::: codeBits(codeTable)(textList.head))
    }

    encode(text, Nil)
  }                                               //> quickEncode: (tree: patmatWorksheet.CodeTree)(text: List[Char])List[patmat
                                                  //| Worksheet.Bit]
  val frenchSecretEncoded = quickEncode(frenchCode)(frenchSecret)
                                                  //> frenchSecretEncoded  : List[patmatWorksheet.Bit] = List(0, 0, 1, 1, 1, 0, 
                                                  //| 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1,
                                                  //|  1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0
                                                  //| , 1, 0, 0, 0, 1, 0, 1)
  decode(frenchCode, frenchSecretEncoded)         //> res30: List[Char] = List(h, u, f, f, m, a, n, e, s, t, c, o, o, l)
  
  makeOrderedLeafList(Nil)                        //> res31: List[patmatWorksheet.Leaf] = List()
  
  combine(Nil)                                    //> res32: List[patmatWorksheet.CodeTree] = List()
}