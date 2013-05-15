package s99

import Solutions._

trait ListsSolutions {

  def last[T](list: List[T]): T = 
    (list: @unchecked) match { case x :: Nil => x case head :: rest => last(rest) }
  def penultimate[T](list: List[T]): T =
    (list: @unchecked) match { case x :: y :: Nil => x case head :: rest => penultimate(rest) }
  def nth[T](n: Int, list: List[T]): T =
    (for((x,i) <- list.zipWithIndex if(i == n)) yield x).head
  def length[T](list: List[T]): Int =
    list match { case head :: rest => 1 + length(rest) case Nil => 0 }
  def reverse[T](list: List[T]): List[T] =
    (List[T]() /: list)((xs, x) => x :: xs)
  def isPalindrome[T](list: List[T]): Boolean =
    list == reverse(list)
  def flatten(list: List[Any]): List[Any] = list match {
      case Nil => Nil 
      case h :: t => h match { 
        case l @ List(_*) => flatten(l) ::: flatten(t) 
        case s => s :: flatten(t)
      } 
  }
  def compress[T](list: List[T]): List[T] = list match { 
      case Nil => Nil 
      case single @ List(x) => single 
      case x::y::rest => if (x == y) compress(x::rest) else x::compress(y::rest) 
  }
  def pack[T](list: List[T]): List[List[T]] = list match {
      case Nil => Nil
      case head :: tail => list.takeWhile(_ == head) :: pack(list.dropWhile(_ == head))
  }
  def encode[T](list: List[T]): List[(Int, T)] = {
    pack(list) map (xs => (xs.size, xs.head))
  }
  def encodeModified[T](list: List[T]): List[Any] = {
    pack(list) map (xs => if (xs.size > 1) (xs.size, xs.head) else xs.head)
  }
  def decode[T](list: List[(Int, T)]): List[T] = list match {
    case Nil => Nil
    case h :: t => (for (i <- 0 until h._1) yield h._2).toList ::: decode(t)
  }
  def encodeDirect[T](list: List[T]): List[(Int, T)] = list match {
    case Nil => Nil
    case h :: t => 
      (list.takeWhile(_ == h).size, h) :: encodeDirect(list.dropWhile(_ == h))
  }
  def duplicate[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case h :: t => h :: h :: duplicate(t)
  }
  def duplicateN[T](n: Int, list: List[T]): List[T] = list match {
    case Nil => Nil
    case h :: t => (for(x <- 0 until n) yield h).toList ::: duplicateN(n, t)
  }
  def drop[T](n: Int, list: List[T]): List[T] = {
    def loop(i: Int, runList: List[T]) : List[T] = runList match {
      case Nil => Nil
      case head :: tail =>
        if (i + 1 == n) loop(0, tail)
        else head :: loop(i + 1, tail)
    }
    
    loop(0, list)
  }
  def split[T](n: Int, list: List[T]): (List[T], List[T]) =
    (list.take(n), list.drop(n))
  def slice[T](i: Int, j: Int, list: List[T]): List[T] = {
    val elemNum = j - i
    def loop(rc: Int, acc: List[T], src: List[T]) : List[T] = {
      if ( acc.size == elemNum ) acc
      else loop(rc + 1, if (rc >= i) src.head :: acc else acc, src.tail)
    }
    
    reverse(loop(0, Nil, list))
  }
  def rotate[T](n: Int, list: List[T]): List[T] = {
    val dn = if (n < 0) list.size + n else n

    list.drop(dn) ::: list.take(dn)
  }
  def removeAt[T](i: Int, list: List[T]): (List[T], T) =
    (slice(0, i, list) ::: slice(i + 1, list.size, list), slice(i, i + 1, list).head)
  def insertAt[T](t: T, i: Int, list: List[T]): List[T] =
    slice(0, i, list) ::: (t :: slice(i, list.size, list))
  def range[T](i: Int, j: Int): List[Int] = {
    def loop(rc: Int, acc: List[Int]) : List[Int] = {
      if (rc == j) rc :: acc
      else loop(rc + 1, rc :: acc)
    }
    
    reverse(loop(i, Nil))
  }
  def randomSelect[T](n: Int, list: List[T]): List[T] = {
    val random = new util.Random()
    
    def loop(acc: List[T], src: List[T]) : List[T] = {
      if (acc.size == n) acc
      else {
        val xs = removeAt(random.nextInt(src.size), src)
        loop(xs._2 :: acc, xs._1)
      }
    }

    loop(Nil, list)
  }
  def lotto[T](i: Int, j: Int): List[Int] = {
    val random = new util.Random()

    def loop(acc: List[Int]) : List[Int] = {
      if (acc.size == i) acc
      else loop(random.nextInt(j) :: acc)
    }

    loop(Nil)
  }
  def randomPermute[T](list: List[T]): List[T] = {
    randomSelect(list.size, list)
  }
  def combinations[T](n: Int, list: List[T]): List[List[T]] = {
    def r[T](rc: Int, ls : List[T]) : List[List[T]] = (rc, ls) match {
      case (0, _) => List(Nil)
      case (_, Nil) => Nil
      case (_, x :: tail) =>
        (r(rc - 1, tail) map (x :: _)) ::: r(rc, tail)
    }

    r(n, list)
  }
  def group3[T](list: List[T]): List[List[List[T]]] = ???
  def groups[T](ns: List[Int], list: List[T]): List[List[List[T]]] = ???
  def lsort[T](list: List[List[T]]): List[List[T]] = ???
  def lsortFreq[T](list: List[List[T]]): List[List[T]] = ???

}

