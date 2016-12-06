package funsets

object funsetsWorksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(86); 
  println("Welcome to the Scala worksheet")
  type Set = Int => Boolean;$skip(148); 
  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem);System.out.println("""contains: (s: funsets.funsetsWorksheet.Set, elem: Int)Boolean""");$skip(119); 

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = (x: Int) => x == elem;System.out.println("""singletonSet: (elem: Int)funsets.funsetsWorksheet.Set""");$skip(21); val res$0 = 
  singletonSet(1)(1);System.out.println("""res0: Boolean = """ + $show(res$0));$skip(179); 
  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = (x: Int) => s(x) || t(x);System.out.println("""union: (s: funsets.funsetsWorksheet.Set, t: funsets.funsetsWorksheet.Set)funsets.funsetsWorksheet.Set""");$skip(38); 
  var s1 = (x: Int) => x > 0 && x < 5;System.out.println("""s1  : Int => Boolean = """ + $show(s1 ));$skip(39); 
  var s2 = (x: Int) => x > 1 && x < 10;System.out.println("""s2  : Int => Boolean = """ + $show(s2 ));$skip(20); val res$1 = 

  union(s1, s2)(1);System.out.println("""res1: Boolean = """ + $show(res$1));$skip(189); 

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = (x: Int) => s(x) && t(x);System.out.println("""intersect: (s: funsets.funsetsWorksheet.Set, t: funsets.funsetsWorksheet.Set)funsets.funsetsWorksheet.Set""");$skip(181); 

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = (x: Int) => s(x) && !t(x);System.out.println("""diff: (s: funsets.funsetsWorksheet.Set, t: funsets.funsetsWorksheet.Set)funsets.funsetsWorksheet.Set""");$skip(137); 

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => s(x) && p(x);System.out.println("""filter: (s: funsets.funsetsWorksheet.Set, p: Int => Boolean)funsets.funsetsWorksheet.Set""");$skip(88); 

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000;System.out.println("""bound  : Int = """ + $show(bound ));$skip(305); 

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) false
      else if (intersect(s, p)(a)) true
      else iter(a + 1)
    }
    iter(-bound)
  };System.out.println("""exists: (s: funsets.funsetsWorksheet.Set, p: Int => Boolean)Boolean""");$skip(332); 

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = {
    (x: Int) =>
      {
        def iter(a: Int): Boolean = {
          if (a > bound) false
          else if (s(a) && f(a) == x) true
          else iter(a + 1)
        }
        iter(-bound)
      }
  };System.out.println("""map: (s: funsets.funsetsWorksheet.Set, f: Int => Int)funsets.funsetsWorksheet.Set""");$skip(40); 
    val s7 = (x: Int) => x > 0 && x < 0;System.out.println("""s7  : Int => Boolean = """ + $show(s7 ));$skip(31); 
    val s8 = (x: Int) => x > 0;System.out.println("""s8  : Int => Boolean = """ + $show(s8 ));$skip(414); 
    
  def forall(s: Set, p: Int => Boolean): Boolean = {
    val sd = diff(s, p)
    
    def isEmpty(a: Int): Boolean = {
      if (a > bound) true
      else if (sd(a)) false
      else isEmpty(a + 1)
    }
    
    val isSetEmpty = isEmpty(-bound)
    
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (sd(a) || isSetEmpty) false
      else iter(a + 1)
    }
    
    iter(-bound)
  };System.out.println("""forall: (s: funsets.funsetsWorksheet.Set, p: Int => Boolean)Boolean""");$skip(20); val res$2 = 
  
  forall(s7, s8);System.out.println("""res2: Boolean = """ + $show(res$2))}
}
