package funsets

object funsetsWorksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  type Set = Int => Boolean
  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)
                                                  //> contains: (s: funsets.funsetsWorksheet.Set, elem: Int)Boolean

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = (x: Int) => x == elem
                                                  //> singletonSet: (elem: Int)funsets.funsetsWorksheet.Set
  singletonSet(1)(1)                              //> res0: Boolean = true
  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = (x: Int) => s(x) || t(x)
                                                  //> union: (s: funsets.funsetsWorksheet.Set, t: funsets.funsetsWorksheet.Set)fun
                                                  //| sets.funsetsWorksheet.Set
  var s1 = (x: Int) => x > 0 && x < 5             //> s1  : Int => Boolean = <function1>
  var s2 = (x: Int) => x > 1 && x < 10            //> s2  : Int => Boolean = <function1>

  union(s1, s2)(1)                                //> res1: Boolean = true

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = (x: Int) => s(x) && t(x)
                                                  //> intersect: (s: funsets.funsetsWorksheet.Set, t: funsets.funsetsWorksheet.Set
                                                  //| )funsets.funsetsWorksheet.Set

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = (x: Int) => s(x) && !t(x)
                                                  //> diff: (s: funsets.funsetsWorksheet.Set, t: funsets.funsetsWorksheet.Set)fun
                                                  //| sets.funsetsWorksheet.Set

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => s(x) && p(x)
                                                  //> filter: (s: funsets.funsetsWorksheet.Set, p: Int => Boolean)funsets.funsets
                                                  //| Worksheet.Set

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000                                //> bound  : Int = 1000

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
  }                                               //> exists: (s: funsets.funsetsWorksheet.Set, p: Int => Boolean)Boolean

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
  }                                               //> map: (s: funsets.funsetsWorksheet.Set, f: Int => Int)funsets.funsetsWorkshe
                                                  //| et.Set
    val s7 = (x: Int) => x > 0 && x < 0           //> s7  : Int => Boolean = <function1>
    val s8 = (x: Int) => x > 0                    //> s8  : Int => Boolean = <function1>
    
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
  }                                               //> forall: (s: funsets.funsetsWorksheet.Set, p: Int => Boolean)Boolean
  
  forall(s7, s8)                                  //> res2: Boolean = false
}