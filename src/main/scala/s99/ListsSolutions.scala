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
  def flatten(list: List[Any]): List[Any] =
    list match { case Nil => Nil case h :: t => h match { case l: List[Any] => flatten(l) ::: flatten(t) case s => s :: flatten(t) } }
  def compress[T](list: List[T]): List[T] = ???
  def pack[T](list: List[T]): List[List[T]] = ???
  def encode[T](list: List[T]): List[(Int, T)] = ???
  def encodeModified[T](list: List[T]): List[Any] = ???
  def decode[T](list: List[(Int, T)]): List[T] = ???
  def encodeDirect[T](list: List[T]): List[(Int, T)] = ???
  def duplicate[T](list: List[T]): List[T] = ???
  def duplicateN[T](n: Int, list: List[T]): List[T] = ???
  def drop[T](n: Int, list: List[T]): List[T] = ???
  def split[T](n: Int, list: List[T]): (List[T], List[T]) = ???
  def slice[T](i: Int, j: Int, list: List[T]): List[T] = ???
  def rotate[T](n: Int, list: List[T]): List[T] = ???
  def removeAt[T](i: Int, list: List[T]): (List[T], T) = ???
  def insertAt[T](t: T, i: Int, list: List[T]): List[T] = ???
  def range[T](i: Int, j: Int): List[Int] = ???
  def randomSelect[T](n: Int, list: List[T]): List[T] = ???
  def lotto[T](i: Int, j: Int): List[Int] = ???
  def randomPermute[T](list: List[T]): List[T] = ???
  def combinations[T](n: Int, list: List[T]): List[List[T]] = ???
  def group3[T](list: List[T]): List[List[List[T]]] = ???
  def groups[T](ns: List[Int], list: List[T]): List[List[List[T]]] = ???
  def lsort[T](list: List[List[T]]): List[List[T]] = ???
  def lsortFreq[T](list: List[List[T]]): List[List[T]] = ???

}

