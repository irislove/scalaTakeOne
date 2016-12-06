package forcomp
import java.io.File

object forcompWorksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(107); 

  println("Welcome to the Scala worksheet")
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
  type Occurrences = List[(Char, Int)];$skip(1137); 

  /**
   * Converts the word into its character occurence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = {
    val wordOccursList = w.toLowerCase().groupBy((char => char)).toList
    for ((char, charList) <- wordOccursList) yield (char, charList.length)
  };System.out.println("""wordOccurrences: (w: forcomp.forcompWorksheet.Word)forcomp.forcompWorksheet.Occurrences""");$skip(29); 

  val word = "testtesttest";System.out.println("""word  : String = """ + $show(word ));$skip(75); 

  val wordList = word.toLowerCase().toList.groupBy((char => char)).toList;System.out.println("""wordList  : List[(Char, List[Char])] = """ + $show(wordList ));$skip(68); val res$0 = 

  for ((char, charList) <- wordList) yield (char, charList.length);System.out.println("""res0: List[(Char, Int)] = """ + $show(res$0));$skip(25); val res$1 = 

  wordOccurrences(word);System.out.println("""res1: forcomp.forcompWorksheet.Occurrences = """ + $show(res$1));$skip(93); 

  def sentenceOccurrences(s: Sentence): Occurrences = {
    wordOccurrences(s.mkString)
  };System.out.println("""sentenceOccurrences: (s: forcomp.forcompWorksheet.Sentence)forcomp.forcompWorksheet.Occurrences""");$skip(47); 

  val sentence = List("test", "test", "test");System.out.println("""sentence  : List[String] = """ + $show(sentence ));$skip(32); val res$2 = 
  sentenceOccurrences(sentence);System.out.println("""res2: forcomp.forcompWorksheet.Occurrences = """ + $show(res$2));$skip(69); 

  val wordMap = Map("1" -> "a", "2" -> "b", "1" -> "d", "2" -> "c");System.out.println("""wordMap  : scala.collection.immutable.Map[String,String] = """ + $show(wordMap ));$skip(50); 

  val words = List("ate", "eat", "tea", "test");System.out.println("""words  : List[String] = """ + $show(words ));$skip(83); ;
  val wordOccurrencesMap = for (word <- words) yield (wordOccurrences(word), word);System.out.println("""wordOccurrencesMap  : List[(forcomp.forcompWorksheet.Occurrences, String)] = """ + $show(wordOccurrencesMap ));$skip(58); 

  val map2 = wordOccurrencesMap.groupBy(list => list._1);System.out.println("""map2  : scala.collection.immutable.Map[forcomp.forcompWorksheet.Occurrences,List[(forcomp.forcompWorksheet.Occurrences, String)]] = """ + $show(map2 ));$skip(80); 

  val wordOccursList = for (word <- words) yield (wordOccurrences(word), word);System.out.println("""wordOccursList  : List[(forcomp.forcompWorksheet.Occurrences, String)] = """ + $show(wordOccursList ));$skip(61); 

  val wordOccursMap = wordOccursList.groupBy(map => map._1);System.out.println("""wordOccursMap  : scala.collection.immutable.Map[forcomp.forcompWorksheet.Occurrences,List[(forcomp.forcompWorksheet.Occurrences, String)]] = """ + $show(wordOccursMap ));$skip(44); val res$3 = 

  wordOccursMap.map(pair => List(pair._2));System.out.println("""res3: scala.collection.immutable.Iterable[List[List[(forcomp.forcompWorksheet.Occurrences, String)]]] = """ + $show(res$3));$skip(67); val res$4 = 

  wordOccursMap.map(map => map._1 -> map._2.map(list => list._2));System.out.println("""res4: scala.collection.immutable.Map[forcomp.forcompWorksheet.Occurrences,List[String]] = """ + $show(res$4));$skip(45); val res$5 = 
  ((1 to 10) map (i => List("a", i))).toList;System.out.println("""res5: List[List[Any]] = """ + $show(res$5));$skip(118); 

  def decomp(pair: (Char, Int)): List[Occurrences] = {
    ((1 to pair._2) map (i => List((pair._1, i)))).toList
  };System.out.println("""decomp: (pair: (Char, Int))List[forcomp.forcompWorksheet.Occurrences]""");$skip(20); val res$6 = 

  decomp(('a', 2));System.out.println("""res6: List[forcomp.forcompWorksheet.Occurrences] = """ + $show(res$6));$skip(23); 

  val pair = ('a', 2);System.out.println("""pair  : (Char, Int) = """ + $show(pair ));$skip(42); val res$7 = 

  (1 to pair._2) map (i => (pair._1, i));System.out.println("""res7: scala.collection.immutable.IndexedSeq[(Char, Int)] = """ + $show(res$7));$skip(54); 

  val occurList = List(('a', 2), ('b', 2), ('c', 3));System.out.println("""occurList  : List[(Char, Int)] = """ + $show(occurList ));$skip(192); 

  def combineOccurrences(leftOccur: Occurrences, rightOccur: Occurrences): List[Occurrences] = {
    for {
      left <- leftOccur
      right <- rightOccur
    } yield List(left, right)
  };System.out.println("""combineOccurrences: (leftOccur: forcomp.forcompWorksheet.Occurrences, rightOccur: forcomp.forcompWorksheet.Occurrences)List[forcomp.forcompWorksheet.Occurrences]""");$skip(196); 

  def combineOccurLists(leftList: List[Occurrences], rightList: List[Occurrences]): List[Occurrences] = {
    for {
      left <- leftList
      right <- rightList
    } yield left ::: right
  };System.out.println("""combineOccurLists: (leftList: List[forcomp.forcompWorksheet.Occurrences], rightList: List[forcomp.forcompWorksheet.Occurrences])List[forcomp.forcompWorksheet.Occurrences]""");$skip(263); 

  def addOccurrences(leftList: List[Occurrences], rightList: List[Occurrences]): List[Occurrences] = rightList match {
    case List(Nil) => Nil
    case y :: ys =>
      for {
        left <- leftList
        right <- rightList
      } yield left ::: right
  };System.out.println("""addOccurrences: (leftList: List[forcomp.forcompWorksheet.Occurrences], rightList: List[forcomp.forcompWorksheet.Occurrences])List[forcomp.forcompWorksheet.Occurrences]""");$skip(246); 
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case Nil => List(Nil)
    case y :: Nil => decomp(y) ::: List(Nil)
    case y :: ys => addOccurrences(decomp(y), combinations(ys)) ::: combinations(ys)
  };System.out.println("""combinations: (occurrences: forcomp.forcompWorksheet.Occurrences)List[forcomp.forcompWorksheet.Occurrences]""");$skip(45); 

  val occurShort = List(('a', 2), ('b', 2));System.out.println("""occurShort  : List[(Char, Int)] = """ + $show(occurShort ));$skip(27); val res$8 = 

  combinations(occurList);System.out.println("""res8: List[forcomp.forcompWorksheet.Occurrences] = """ + $show(res$8));$skip(30); 
  val single = List(('a', 2));System.out.println("""single  : List[(Char, Int)] = """ + $show(single ));$skip(23); val res$9 = 
  combinations(single);System.out.println("""res9: List[forcomp.forcompWorksheet.Occurrences] = """ + $show(res$9));$skip(20); val res$10 = 
  combinations(Nil);System.out.println("""res10: List[forcomp.forcompWorksheet.Occurrences] = """ + $show(res$10));$skip(27); val res$11 = 
  combinations(occurShort);System.out.println("""res11: List[forcomp.forcompWorksheet.Occurrences] = """ + $show(res$11));$skip(412); 

  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val xMap = x.toMap withDefaultValue 0
    val yMap = y.toMap withDefaultValue 0

    def subtractTerms(terms: Map[Char, Int], term: (Char, Int)): Map[Char, Int] = {
      val (char, occur) = term

      terms.updated(char, (terms(char) - occur))
    }

    (((yMap foldLeft xMap)(subtractTerms)).toList.filter(pair => pair._2 > 0)).sorted
  };System.out.println("""subtract: (x: forcomp.forcompWorksheet.Occurrences, y: forcomp.forcompWorksheet.Occurrences)forcomp.forcompWorksheet.Occurrences""");$skip(59); 

  val left = List(('d', 1), ('l', 1), ('r', 2), ('a', 1));System.out.println("""left  : List[(Char, Int)] = """ + $show(left ));$skip(35); 
  val y = List(('r', 1), ('l', 1));System.out.println("""y  : List[(Char, Int)] = """ + $show(y ));$skip(21); val res$12 = 

  subtract(left, y);System.out.println("""res12: forcomp.forcompWorksheet.Occurrences = """ + $show(res$12));$skip(41); 

  val sentenceTest = List("Yes", "man");System.out.println("""sentenceTest  : List[String] = """ + $show(sentenceTest ));$skip(36); val res$13 = 
  sentenceOccurrences(sentenceTest)


  /**
   * An alias for the `Nothing` type.
   *  Denotes that the type should be filled in.
   */
  type ??? = Nothing

  /**
   * An alias for the `Any` type.
   *  Denotes that the type should be filled in.
   */
  type *** = Any;System.out.println("""res13: forcomp.forcompWorksheet.Occurrences = """ + $show(res$13));$skip(480); 

  /**
   * Get a child of a file. For example,
   *
   *   subFile(homeDir, "b", "c")
   *
   * corresponds to ~/b/c
   */
  def subFile(file: File, children: String*) = {
    children.foldLeft(file)((file, child) => new File(file, child))
  };System.out.println("""subFile: (file: java.io.File, children: String*)java.io.File""");$skip(622); 

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
  };System.out.println("""resourceAsStreamFromSrc: (resourcePath: List[String])Option[java.io.InputStream]""");$skip(58); 

  val dictionaryPath = List("forcomp", "linuxwords.txt");System.out.println("""dictionaryPath  : List[String] = """ + $show(dictionaryPath ));$skip(536); 

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
  };System.out.println("""loadDictionary: => List[String]""");$skip(47); 

  val dictionary: List[Word] = loadDictionary;System.out.println("""dictionary  : List[forcomp.forcompWorksheet.Word] = """ + $show(dictionary ));$skip(300); 

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    val wordOccursList = for (word <- dictionary) yield (wordOccurrences(word), word)
    val wordOccursMap = wordOccursList.groupBy(wordList => wordList._1)
    wordOccursMap.map(map => map._1 -> map._2.map(list => list._2))
  };System.out.println("""dictionaryByOccurrences: => Map[forcomp.forcompWorksheet.Occurrences,List[forcomp.forcompWorksheet.Word]]""");$skip(262); 
  def combineSentences(sentence: Sentence, sentenceList: List[Sentence]): List[Sentence] = sentenceList match {
    case Nil => List(sentence)
    case y :: ys =>
      for {
        left <- sentence
        right <- sentenceList
      } yield left :: right
  };System.out.println("""combineSentences: (sentence: forcomp.forcompWorksheet.Sentence, sentenceList: List[forcomp.forcompWorksheet.Sentence])List[forcomp.forcompWorksheet.Sentence]""");$skip(207); 
  
    def sameOccurrences(occurrencesLeft: Occurrences, occurrencesRight: Occurrences): Boolean = {
    val map = occurrencesRight.toMap
    occurrencesLeft.forall((occur => occur._2 == map(occur._1)))
  };System.out.println("""sameOccurrences: (occurrencesLeft: forcomp.forcompWorksheet.Occurrences, occurrencesRight: forcomp.forcompWorksheet.Occurrences)Boolean""");$skip(71); 
  val wordOccursMaptest = dictionaryByOccurrences withDefaultValue Nil;System.out.println("""wordOccursMaptest  : scala.collection.immutable.Map[forcomp.forcompWorksheet.Occurrences,List[forcomp.forcompWorksheet.Word]] = """ + $show(wordOccursMaptest ));$skip(641); 
  
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
  };System.out.println("""sentenceAnagrams: (sentence: forcomp.forcompWorksheet.Sentence)List[forcomp.forcompWorksheet.Sentence]""");$skip(45); 

  val testSentence = List("Linux", "rulez");System.out.println("""testSentence  : List[String] = """ + $show(testSentence ));$skip(34); val res$14 = 
sentenceOccurrences(testSentence);System.out.println("""res14: forcomp.forcompWorksheet.Occurrences = """ + $show(res$14));$skip(50); val res$15 = 
  combinations(sentenceOccurrences(testSentence));System.out.println("""res15: List[forcomp.forcompWorksheet.Occurrences] = """ + $show(res$15));$skip(145); val res$16 = 
  //generateAnagrams(List(('x',1)))
  //val wordOccursMaptest = dictionaryByOccurrences withDefaultValue (Nil)

  sentenceAnagrams(testSentence);System.out.println("""res16: List[forcomp.forcompWorksheet.Sentence] = """ + $show(res$16));$skip(64); val res$17 = 
  
  
  combinations(sentenceOccurrences(List("olive", "you")));System.out.println("""res17: List[forcomp.forcompWorksheet.Occurrences] = """ + $show(res$17));$skip(32); val res$18 = 
  sentenceAnagrams(List("and"));System.out.println("""res18: List[forcomp.forcompWorksheet.Sentence] = """ + $show(res$18));$skip(39); val res$19 = 
  sentenceAnagrams(List("yes", "man"));System.out.println("""res19: List[forcomp.forcompWorksheet.Sentence] = """ + $show(res$19));$skip(25); val res$20 = 
sentenceAnagrams(List());System.out.println("""res20: List[forcomp.forcompWorksheet.Sentence] = """ + $show(res$20))}
}
