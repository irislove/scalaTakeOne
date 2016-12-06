package forcomp
import java.io.File

package object forcomp {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(118); 
  val dictionaryPath = List("forcomp", "linuxwords.txt");System.out.println("""dictionaryPath  : List[String] = """ + $show(dictionaryPath ));$skip(543); 

  def loadDictionary = {
    val wordstream = Option {
      getClass.getClassLoader.getResourceAsStream(dictionaryPath.mkString("/"))
    } orElse {
      common.resourceAsStreamFromSrc(dictionaryPath)
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
  };System.out.println("""loadDictionary: => List[String]""")}

}

package object common {

  /** An alias for the `Nothing` type.
   *  Denotes that the type should be filled in.
   */
  type ??? = Nothing

  /** An alias for the `Any` type.
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
  }

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
  }
}

object forcompWorksheet {

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
  }

  val word = "testtesttest"

  val wordList = word.toLowerCase().toList.groupBy((char => char)).toList

  for ((char, charList) <- wordList) yield (char, charList.length)

  wordOccurrences(word)

  def sentenceOccurrences(s: Sentence): Occurrences = {
    wordOccurrences(s.mkString)
  }

  val sentence = List("test", "test", "test")
  sentenceOccurrences(sentence)

  val wordMap = Map("1" -> "a", "2" -> "b", "1" -> "d", "2" -> "c")

  val words = List("ate", "eat", "tea", "test");
  val wordOccurrencesMap = for (word <- words) yield (wordOccurrences(word), word)

  val map2 = wordOccurrencesMap.groupBy(list => list._1)

  val wordOccursList = for (word <- words) yield (wordOccurrences(word), word)

  val wordOccursMap = wordOccursList.groupBy(map => map._1)

  wordOccursMap.map(pair => List(pair._2))

  wordOccursMap.map(map => map._1 -> map._2.map(list => list._2))
  ((1 to 10) map (i => List("a", i))).toList

  def decomp(pair: (Char, Int)): List[Occurrences] = {
    ((1 to pair._2) map (i => List((pair._1, i)))).toList
  }

  decomp(('a', 2))

  val pair = ('a', 2)

  (1 to pair._2) map (i => (pair._1, i))

  val occurList = List(('a', 2), ('b', 2), ('c', 3))

  def combineOccurrences(leftOccur: Occurrences, rightOccur: Occurrences): List[Occurrences] = {
    for {
      left <- leftOccur
      right <- rightOccur
    } yield List(left, right)
  }

  def combineOccurLists(leftList: List[Occurrences], rightList: List[Occurrences]): List[Occurrences] = {
    for {
      left <- leftList
      right <- rightList
    } yield left ::: right
  }

  def addOccurrences(leftList: List[Occurrences], rightList: List[Occurrences]): List[Occurrences] = rightList match {
    case List(Nil) => Nil
    case y :: ys =>
      for {
        left <- leftList
        right <- rightList
      } yield left ::: right
  }
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case Nil => List(Nil)
    case y :: Nil => decomp(y) ::: List(Nil)
    case y :: ys => addOccurrences(decomp(y), combinations(ys)) ::: combinations(ys)
  }

  val occurShort = List(('a', 2), ('b', 2))

  combinations(occurList)
  val single = List(('a', 2))
  combinations(single)
  combinations(Nil)
  combinations(occurShort)

  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val xMap = x.toMap withDefaultValue 0
    val yMap = y.toMap withDefaultValue 0
    
    def subtractTerms(terms: Map[Char, Int], term: (Char, Int)): Map[Char, Int] = {
      val (char, occur) = term
      
      terms.updated(char, (terms(char) - occur))
    }
    
    (((yMap foldLeft xMap)(subtractTerms)).toList.filter(pair => pair._2  > 0)).sorted
  }

	val left = List(('d', 1), ('l', 1), ('r', 2), ('a', 1))
	val y = List(('r', 1), ('l', 1))
	
	
	
	subtract(left, y)
	
	val sentenceTest = List("Yes", "man")
	sentenceOccurrences(sentenceTest)



  def combineSentences(sentence: Sentence, sentenceList: List[Sentence]): List[Sentence] = sentenceList match {
    case Nil => List(sentence)
    case y :: ys =>
      for {
    	  left <- sentence
    	  right <- sentenceList
      } yield left :: right
  }
  
  val dictionary: List[Word] = List()
  
   lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    val wordOccursList = for(word <- dictionary) yield (wordOccurrences(word), word)
    val wordOccursMap = wordOccursList.groupBy(wordList => wordList._1)
    wordOccursMap.map(map => map._1 -> map._2.map(list => list._2))
  }
  
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    val wordOccursMap = dictionaryByOccurrences withDefaultValue (Nil)
    
    def generateAnagrams(occurrences: Occurrences): List[Sentence] = occurrences match {
      case Nil => List(Nil)
      case y :: ys =>
        val occurCombinations = combinations(occurrences)
    	(
          for {
        	  occurs <- occurCombinations
          } yield combineSentences(wordOccursMap(occurs), generateAnagrams(subtract(occurrences, occurs)))
    	).flatten
    }
    
    generateAnagrams(sentenceOccurrences(sentence))
  }
}
