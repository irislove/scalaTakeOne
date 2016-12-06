package forcomp
import java.io.File

object forcompWorksheet {

  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /**
   * `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /**
   * Converts the word into its character occurence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = {
    val wordOccursList = w.toLowerCase().groupBy((char => char)).toList
    for ((char, charList) <- wordOccursList) yield (char, charList.length)
  }                                               //> wordOccurrences: (w: forcomp.forcompWorksheet.Word)forcomp.forcompWorksheet
                                                  //| .Occurrences

  val word = "testtesttest"                       //> word  : String = testtesttest

  val wordList = word.toLowerCase().toList.groupBy((char => char)).toList
                                                  //> wordList  : List[(Char, List[Char])] = List((e,List(e, e, e)), (t,List(t, t
                                                  //| , t, t, t, t)), (s,List(s, s, s)))

  for ((char, charList) <- wordList) yield (char, charList.length)
                                                  //> res0: List[(Char, Int)] = List((e,3), (t,6), (s,3))

  wordOccurrences(word)                           //> res1: forcomp.forcompWorksheet.Occurrences = List((e,3), (t,6), (s,3))

  def sentenceOccurrences(s: Sentence): Occurrences = {
    wordOccurrences(s.mkString)
  }                                               //> sentenceOccurrences: (s: forcomp.forcompWorksheet.Sentence)forcomp.forcompW
                                                  //| orksheet.Occurrences

  val sentence = List("test", "test", "test")     //> sentence  : List[String] = List(test, test, test)
  sentenceOccurrences(sentence)                   //> res2: forcomp.forcompWorksheet.Occurrences = List((e,3), (t,6), (s,3))

  val wordMap = Map("1" -> "a", "2" -> "b", "1" -> "d", "2" -> "c")
                                                  //> wordMap  : scala.collection.immutable.Map[String,String] = Map(1 -> d, 2 ->
                                                  //|  c)

  val words = List("ate", "eat", "tea", "test");  //> words  : List[String] = List(ate, eat, tea, test)
  val wordOccurrencesMap = for (word <- words) yield (wordOccurrences(word), word)
                                                  //> wordOccurrencesMap  : List[(forcomp.forcompWorksheet.Occurrences, String)] 
                                                  //| = List((List((e,1), (t,1), (a,1)),ate), (List((e,1), (t,1), (a,1)),eat), (L
                                                  //| ist((e,1), (t,1), (a,1)),tea), (List((e,1), (t,2), (s,1)),test))

  val map2 = wordOccurrencesMap.groupBy(list => list._1)
                                                  //> map2  : scala.collection.immutable.Map[forcomp.forcompWorksheet.Occurrences
                                                  //| ,List[(forcomp.forcompWorksheet.Occurrences, String)]] = Map(List((e,1), (t
                                                  //| ,1), (a,1)) -> List((List((e,1), (t,1), (a,1)),ate), (List((e,1), (t,1), (a
                                                  //| ,1)),eat), (List((e,1), (t,1), (a,1)),tea)), List((e,1), (t,2), (s,1)) -> L
                                                  //| ist((List((e,1), (t,2), (s,1)),test)))

  val wordOccursList = for (word <- words) yield (wordOccurrences(word), word)
                                                  //> wordOccursList  : List[(forcomp.forcompWorksheet.Occurrences, String)] = Li
                                                  //| st((List((e,1), (t,1), (a,1)),ate), (List((e,1), (t,1), (a,1)),eat), (List(
                                                  //| (e,1), (t,1), (a,1)),tea), (List((e,1), (t,2), (s,1)),test))

  val wordOccursMap = wordOccursList.groupBy(map => map._1)
                                                  //> wordOccursMap  : scala.collection.immutable.Map[forcomp.forcompWorksheet.Oc
                                                  //| currences,List[(forcomp.forcompWorksheet.Occurrences, String)]] = Map(List(
                                                  //| (e,1), (t,1), (a,1)) -> List((List((e,1), (t,1), (a,1)),ate), (List((e,1), 
                                                  //| (t,1), (a,1)),eat), (List((e,1), (t,1), (a,1)),tea)), List((e,1), (t,2), (s
                                                  //| ,1)) -> List((List((e,1), (t,2), (s,1)),test)))

  wordOccursMap.map(pair => List(pair._2))        //> res3: scala.collection.immutable.Iterable[List[List[(forcomp.forcompWorkshe
                                                  //| et.Occurrences, String)]]] = List(List(List((List((e,1), (t,1), (a,1)),ate)
                                                  //| , (List((e,1), (t,1), (a,1)),eat), (List((e,1), (t,1), (a,1)),tea))), List(
                                                  //| List((List((e,1), (t,2), (s,1)),test))))

  wordOccursMap.map(map => map._1 -> map._2.map(list => list._2))
                                                  //> res4: scala.collection.immutable.Map[forcomp.forcompWorksheet.Occurrences,L
                                                  //| ist[String]] = Map(List((e,1), (t,1), (a,1)) -> List(ate, eat, tea), List((
                                                  //| e,1), (t,2), (s,1)) -> List(test))
  ((1 to 10) map (i => List("a", i))).toList      //> res5: List[List[Any]] = List(List(a, 1), List(a, 2), List(a, 3), List(a, 4)
                                                  //| , List(a, 5), List(a, 6), List(a, 7), List(a, 8), List(a, 9), List(a, 10))

  def decomp(pair: (Char, Int)): List[Occurrences] = {
    ((1 to pair._2) map (i => List((pair._1, i)))).toList
  }                                               //> decomp: (pair: (Char, Int))List[forcomp.forcompWorksheet.Occurrences]

  decomp(('a', 2))                                //> res6: List[forcomp.forcompWorksheet.Occurrences] = List(List((a,1)), List((
                                                  //| a,2)))

  val pair = ('a', 2)                             //> pair  : (Char, Int) = (a,2)

  (1 to pair._2) map (i => (pair._1, i))          //> res7: scala.collection.immutable.IndexedSeq[(Char, Int)] = Vector((a,1), (a
                                                  //| ,2))

  val occurList = List(('a', 2), ('b', 2), ('c', 3))
                                                  //> occurList  : List[(Char, Int)] = List((a,2), (b,2), (c,3))

  def combineOccurrences(leftOccur: Occurrences, rightOccur: Occurrences): List[Occurrences] = {
    for {
      left <- leftOccur
      right <- rightOccur
    } yield List(left, right)
  }                                               //> combineOccurrences: (leftOccur: forcomp.forcompWorksheet.Occurrences, right
                                                  //| Occur: forcomp.forcompWorksheet.Occurrences)List[forcomp.forcompWorksheet.O
                                                  //| ccurrences]

  def combineOccurLists(leftList: List[Occurrences], rightList: List[Occurrences]): List[Occurrences] = {
    for {
      left <- leftList
      right <- rightList
    } yield left ::: right
  }                                               //> combineOccurLists: (leftList: List[forcomp.forcompWorksheet.Occurrences], r
                                                  //| ightList: List[forcomp.forcompWorksheet.Occurrences])List[forcomp.forcompWo
                                                  //| rksheet.Occurrences]

  def addOccurrences(leftList: List[Occurrences], rightList: List[Occurrences]): List[Occurrences] = rightList match {
    case List(Nil) => Nil
    case y :: ys =>
      for {
        left <- leftList
        right <- rightList
      } yield left ::: right
  }                                               //> addOccurrences: (leftList: List[forcomp.forcompWorksheet.Occurrences], righ
                                                  //| tList: List[forcomp.forcompWorksheet.Occurrences])List[forcomp.forcompWorks
                                                  //| heet.Occurrences]
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case Nil => List(Nil)
    case y :: Nil => decomp(y) ::: List(Nil)
    case y :: ys => addOccurrences(decomp(y), combinations(ys)) ::: combinations(ys)
  }                                               //> combinations: (occurrences: forcomp.forcompWorksheet.Occurrences)List[forco
                                                  //| mp.forcompWorksheet.Occurrences]

  val occurShort = List(('a', 2), ('b', 2))       //> occurShort  : List[(Char, Int)] = List((a,2), (b,2))

  combinations(occurList)                         //> res8: List[forcomp.forcompWorksheet.Occurrences] = List(List((a,1), (b,1), 
                                                  //| (c,1)), List((a,1), (b,1), (c,2)), List((a,1), (b,1), (c,3)), List((a,1), (
                                                  //| b,1)), List((a,1), (b,2), (c,1)), List((a,1), (b,2), (c,2)), List((a,1), (b
                                                  //| ,2), (c,3)), List((a,1), (b,2)), List((a,1), (c,1)), List((a,1), (c,2)), Li
                                                  //| st((a,1), (c,3)), List((a,1)), List((a,2), (b,1), (c,1)), List((a,2), (b,1)
                                                  //| , (c,2)), List((a,2), (b,1), (c,3)), List((a,2), (b,1)), List((a,2), (b,2),
                                                  //|  (c,1)), List((a,2), (b,2), (c,2)), List((a,2), (b,2), (c,3)), List((a,2), 
                                                  //| (b,2)), List((a,2), (c,1)), List((a,2), (c,2)), List((a,2), (c,3)), List((a
                                                  //| ,2)), List((b,1), (c,1)), List((b,1), (c,2)), List((b,1), (c,3)), List((b,1
                                                  //| )), List((b,2), (c,1)), List((b,2), (c,2)), List((b,2), (c,3)), List((b,2))
                                                  //| , List((c,1)), List((c,2)), List((c,3)), List())
  val single = List(('a', 2))                     //> single  : List[(Char, Int)] = List((a,2))
  combinations(single)                            //> res9: List[forcomp.forcompWorksheet.Occurrences] = List(List((a,1)), List((
                                                  //| a,2)), List())
  combinations(Nil)                               //> res10: List[forcomp.forcompWorksheet.Occurrences] = List(List())
  combinations(occurShort)                        //> res11: List[forcomp.forcompWorksheet.Occurrences] = List(List((a,1), (b,1))
                                                  //| , List((a,1), (b,2)), List((a,1)), List((a,2), (b,1)), List((a,2), (b,2)), 
                                                  //| List((a,2)), List((b,1)), List((b,2)), List())

  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val xMap = x.toMap withDefaultValue 0
    val yMap = y.toMap withDefaultValue 0

    def subtractTerms(terms: Map[Char, Int], term: (Char, Int)): Map[Char, Int] = {
      val (char, occur) = term

      terms.updated(char, (terms(char) - occur))
    }

    (((yMap foldLeft xMap)(subtractTerms)).toList.filter(pair => pair._2 > 0)).sorted
  }                                               //> subtract: (x: forcomp.forcompWorksheet.Occurrences, y: forcomp.forcompWorks
                                                  //| heet.Occurrences)forcomp.forcompWorksheet.Occurrences

  val left = List(('d', 1), ('l', 1), ('r', 2), ('a', 1))
                                                  //> left  : List[(Char, Int)] = List((d,1), (l,1), (r,2), (a,1))
  val y = List(('r', 1), ('l', 1))                //> y  : List[(Char, Int)] = List((r,1), (l,1))

  subtract(left, y)                               //> res12: forcomp.forcompWorksheet.Occurrences = List((a,1), (d,1), (r,1))

  val sentenceTest = List("Yes", "man")           //> sentenceTest  : List[String] = List(Yes, man)
  sentenceOccurrences(sentenceTest)               //> res13: forcomp.forcompWorksheet.Occurrences = List((e,1), (s,1), (n,1), (y,
                                                  //| 1), (a,1), (m,1))


  /**
   * An alias for the `Nothing` type.
   *  Denotes that the type should be filled in.
   */
  type ??? = Nothing

  /**
   * An alias for the `Any` type.
   *  Denotes that the type should be filled in.
   */
  type *** = Any

  /**
   * Get a child of a file. For example,
   *
   *   subFile(homeDir, "b", "c")
   *
   * corresponds to ~/b/c
   */
  def subFile(file: File, children: String*) = {
    children.foldLeft(file)((file, child) => new File(file, child))
  }                                               //> subFile: (file: java.io.File, children: String*)java.io.File

  /**
   * Get a resource from the `src/main/resources` directory. Eclipse does not copy
   * resources to the output directory, then the class loader cannot find them.
   */
  def resourceAsStreamFromSrc(resourcePath: List[String]): Option[java.io.InputStream] = {
    val classesDir = new File(getClass.getResource(".").toURI)
    val projectDir = classesDir.getParentFile.getParentFile.getParentFile.getParentFile
    val resourceFile = subFile(projectDir, ("src" :: "main" :: "resources" :: resourcePath): _*)
    if (resourceFile.exists)
      Some(new java.io.FileInputStream(resourceFile))
    else
      None
  }                                               //> resourceAsStreamFromSrc: (resourcePath: List[String])Option[java.io.InputSt
                                                  //| ream]

  val dictionaryPath = List("forcomp", "linuxwords.txt")
                                                  //> dictionaryPath  : List[String] = List(forcomp, linuxwords.txt)

  def loadDictionary = {
    val wordstream = Option {
      getClass.getClassLoader.getResourceAsStream(dictionaryPath.mkString("/"))
    } orElse {
      resourceAsStreamFromSrc(dictionaryPath)
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }                                               //> loadDictionary: => List[String]

  val dictionary: List[Word] = loadDictionary     //> dictionary  : List[forcomp.forcompWorksheet.Word] = List(Aarhus, Aaron, Aba
                                                  //| ba, aback, abaft, abandon, abandoned, abandoning, abandonment, abandons, ab
                                                  //| ase, abased, abasement, abasements, abases, abash, abashed, abashes, abashi
                                                  //| ng, abasing, abate, abated, abatement, abatements, abater, abates, abating,
                                                  //|  Abba, abbe, abbey, abbeys, abbot, abbots, Abbott, abbreviate, abbreviated,
                                                  //|  abbreviates, abbreviating, abbreviation, abbreviations, Abby, abdomen, abd
                                                  //| omens, abdominal, abduct, abducted, abduction, abductions, abductor, abduct
                                                  //| ors, abducts, Abe, abed, Abel, Abelian, Abelson, Aberdeen, Abernathy, aberr
                                                  //| ant, aberration, aberrations, abet, abets, abetted, abetter, abetting, abey
                                                  //| ance, abhor, abhorred, abhorrent, abhorrer, abhorring, abhors, abide, abide
                                                  //| d, abides, abiding, Abidjan, Abigail, Abilene, abilities, ability, abject, 
                                                  //| abjection, abjections, abjectly, abjectness, abjure, abjured, abjures, abju
                                                  //| ring, ablate, ablated, 
                                                  //| Output exceeds cutoff limit.

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    val wordOccursList = for (word <- dictionary) yield (wordOccurrences(word), word)
    val wordOccursMap = wordOccursList.groupBy(wordList => wordList._1)
    wordOccursMap.map(map => map._1 -> map._2.map(list => list._2))
  }                                               //> dictionaryByOccurrences: => Map[forcomp.forcompWorksheet.Occurrences,List[f
                                                  //| orcomp.forcompWorksheet.Word]]
  def combineSentences(sentence: Sentence, sentenceList: List[Sentence]): List[Sentence] = sentenceList match {
    case Nil => List(sentence)
    case y :: ys =>
      for {
        left <- sentence
        right <- sentenceList
      } yield left :: right
  }                                               //> combineSentences: (sentence: forcomp.forcompWorksheet.Sentence, sentenceLis
                                                  //| t: List[forcomp.forcompWorksheet.Sentence])List[forcomp.forcompWorksheet.Se
                                                  //| ntence]
  
    def sameOccurrences(occurrencesLeft: Occurrences, occurrencesRight: Occurrences): Boolean = {
    val map = occurrencesRight.toMap
    occurrencesLeft.forall((occur => occur._2 == map(occur._1)))
  }                                               //> sameOccurrences: (occurrencesLeft: forcomp.forcompWorksheet.Occurrences, oc
                                                  //| currencesRight: forcomp.forcompWorksheet.Occurrences)Boolean
  val wordOccursMaptest = dictionaryByOccurrences withDefaultValue Nil
                                                  //> wordOccursMaptest  : scala.collection.immutable.Map[forcomp.forcompWorkshee
                                                  //| t.Occurrences,List[forcomp.forcompWorksheet.Word]] = Map(List((u,1), (b,1),
                                                  //|  (c,1), (h,1), (r,1)) -> List(Burch), List((e,1), (s,1), (t,1), (u,1), (a,1
                                                  //| ), (l,1), (p,2), (o,1)) -> List(populates), List((e,2), (s,2), (n,1), (a,1)
                                                  //| , (r,1), (o,1)) -> List(seasoner), List((s,1), (u,1), (a,1), (b,2), (c,1), 
                                                  //| (r,1), (k,1), (o,1), (d,1)) -> List(buckboards), List((n,1), (h,1), (s,1), 
                                                  //| (u,1)) -> List(Huns, shun), List((e,2), (s,2), (t,1), (u,1), (a,1), (i,1), 
                                                  //| (g,1), (l,2), (r,1)) -> List(legislatures), List((s,1), (n,1), (t,1), (u,1)
                                                  //| , (a,1), (l,1), (w,1)) -> List(walnuts), List((e,1), (a,1), (m,1), (g,1), (
                                                  //| l,1)) -> List(gleam), List((t,1), (a,1), (l,1), (h,1), (o,1)) -> List(loath
                                                  //| ), List((s,1), (n,1), (p,1), (h,1), (r,1), (w,1), (o,2)) -> List(shopworn),
                                                  //|  List((e,2), (y,1), (v,1), (l,2), (c,1), (r,1)) -> List(cleverly), List((e,
                                                  //| 2), (n,1), (f,1), (a,1)
                                                  //| Output exceeds cutoff limit.
  
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    val sentenceOccur = sentenceOccurrences(sentence)
    val sentenceOccursMap = dictionaryByOccurrences withDefaultValue Nil
    
    def generateAnagrams(occurrences: Occurrences): List[Sentence] = {
      if (occurrences.isEmpty) List(Nil)
      else {
      val sentenceCombinations = combinations(occurrences)
        for {
          occur <- sentenceCombinations
          sentence <- sentenceOccursMap(occur)
          rest <- generateAnagrams(subtract(occurrences, occur))
        } yield sentence :: rest
      }
    }

    
    generateAnagrams(sentenceOccur)
  }                                               //> sentenceAnagrams: (sentence: forcomp.forcompWorksheet.Sentence)List[forcomp
                                                  //| .forcompWorksheet.Sentence]

  val testSentence = List("Linux", "rulez")       //> testSentence  : List[String] = List(Linux, rulez)
sentenceOccurrences(testSentence)                 //> res14: forcomp.forcompWorksheet.Occurrences = List((e,1), (x,1), (n,1), (u,
                                                  //| 2), (i,1), (l,2), (r,1), (z,1))
  combinations(sentenceOccurrences(testSentence)) //> res15: List[forcomp.forcompWorksheet.Occurrences] = List(List((e,1), (x,1),
                                                  //|  (n,1), (u,1), (i,1), (l,1), (r,1), (z,1)), List((e,1), (x,1), (n,1), (u,1)
                                                  //| , (i,1), (l,1), (r,1)), List((e,1), (x,1), (n,1), (u,1), (i,1), (l,1), (z,1
                                                  //| )), List((e,1), (x,1), (n,1), (u,1), (i,1), (l,1)), List((e,1), (x,1), (n,1
                                                  //| ), (u,1), (i,1), (l,2), (r,1), (z,1)), List((e,1), (x,1), (n,1), (u,1), (i,
                                                  //| 1), (l,2), (r,1)), List((e,1), (x,1), (n,1), (u,1), (i,1), (l,2), (z,1)), L
                                                  //| ist((e,1), (x,1), (n,1), (u,1), (i,1), (l,2)), List((e,1), (x,1), (n,1), (u
                                                  //| ,1), (i,1), (r,1), (z,1)), List((e,1), (x,1), (n,1), (u,1), (i,1), (r,1)), 
                                                  //| List((e,1), (x,1), (n,1), (u,1), (i,1), (z,1)), List((e,1), (x,1), (n,1), (
                                                  //| u,1), (i,1)), List((e,1), (x,1), (n,1), (u,1), (l,1), (r,1), (z,1)), List((
                                                  //| e,1), (x,1), (n,1), (u,1), (l,1), (r,1)), List((e,1), (x,1), (n,1), (u,1), 
                                                  //| (l,1), (z,1)), List((e,1), (x,1), (n,1), (u,1), (l,1)), List((e,1), (x,1), 
                                                  //| (n,1), (u,1), (l,2), (r
                                                  //| Output exceeds cutoff limit.
  //generateAnagrams(List(('x',1)))
  //val wordOccursMaptest = dictionaryByOccurrences withDefaultValue (Nil)

  sentenceAnagrams(testSentence)                  //> res16: List[forcomp.forcompWorksheet.Sentence] = List()
  
  
  combinations(sentenceOccurrences(List("olive", "you")))
                                                  //> res17: List[forcomp.forcompWorksheet.Occurrences] = List(List((e,1), (y,1),
                                                  //|  (u,1), (i,1), (v,1), (l,1), (o,1)), List((e,1), (y,1), (u,1), (i,1), (v,1)
                                                  //| , (l,1), (o,2)), List((e,1), (y,1), (u,1), (i,1), (v,1), (l,1)), List((e,1)
                                                  //| , (y,1), (u,1), (i,1), (v,1), (o,1)), List((e,1), (y,1), (u,1), (i,1), (v,1
                                                  //| ), (o,2)), List((e,1), (y,1), (u,1), (i,1), (v,1)), List((e,1), (y,1), (u,1
                                                  //| ), (i,1), (l,1), (o,1)), List((e,1), (y,1), (u,1), (i,1), (l,1), (o,2)), Li
                                                  //| st((e,1), (y,1), (u,1), (i,1), (l,1)), List((e,1), (y,1), (u,1), (i,1), (o,
                                                  //| 1)), List((e,1), (y,1), (u,1), (i,1), (o,2)), List((e,1), (y,1), (u,1), (i,
                                                  //| 1)), List((e,1), (y,1), (u,1), (v,1), (l,1), (o,1)), List((e,1), (y,1), (u,
                                                  //| 1), (v,1), (l,1), (o,2)), List((e,1), (y,1), (u,1), (v,1), (l,1)), List((e,
                                                  //| 1), (y,1), (u,1), (v,1), (o,1)), List((e,1), (y,1), (u,1), (v,1), (o,2)), L
                                                  //| ist((e,1), (y,1), (u,1), (v,1)), List((e,1), (y,1), (u,1), (l,1), (o,1)), L
                                                  //| ist((e,1), (y,1), (u,1)
                                                  //| Output exceeds cutoff limit.
  sentenceAnagrams(List("and"))                   //> res18: List[forcomp.forcompWorksheet.Sentence] = List(List(and), List(Dan))
                                                  //| 
  sentenceAnagrams(List("yes", "man"))            //> res19: List[forcomp.forcompWorksheet.Sentence] = List()
sentenceAnagrams(List())                          //> res20: List[forcomp.forcompWorksheet.Sentence] = List(List())
}