package s99

import Solutions.???

trait ArithmeticSolutions {

  // add new functions to integers
  implicit def extendInt(n: Int): ExtendedInt = ExtendedInt(n)

  case class ExtendedInt(n: Int) {

    def isPrime: Boolean = listPrimesinRange(2 to n).contains(n)
    def isCoprimeTo(n: Int): Boolean = gcd(this.n, n) == 1
    def totient: Int = (1 until n).count(x => gcd(n, x) == 1)
    def improvedTotient: Int = {
      (1 /: primeFactorMultiplicity) ((x, y) => x * (y._1 - 1) * math.pow(y._1, y._2 - 1).toInt)
    }
    def primeFactors: List[Int] = {
      def r(x: Int, primes: List[Int]) : List[Int] = {
        primes.find(x % _ == 0) match {
          case None => x::Nil
          case Some(factor) => if (factor == x) factor:: Nil else factor :: r(x/factor, primes)
        }
      }
      r(n, listPrimesinRange(2 to n))
    }
    def primeFactorMultiplicity: List[(Int, Int)] = {
      def r(c: Int, factors: List[Int], acc: List[(Int, Int)]) : List[(Int, Int)] = factors match {
        case Nil => acc
        case x::t => if (c == x) r(c,t,acc) else r(x,t,(x, factors.count(_==x))::acc)
      }
      r(0, primeFactors, Nil).reverse
    }
    def primeFactorMultiplicityMap: Map[Int, Int] = primeFactorMultiplicity.toMap
    def goldbach: (Int, Int) = ???

  }

  def primes: Stream[Int] = ???
  def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)
  def listPrimesinRange(rg: Range): List[Int] = {
      @annotation.tailrec
      def r(s: Int, ls: List[Int], acc: List[Int]) : List[Int] = {
        val ps = ls.filter(_ % s != 0)
        if (ps == ls) (s :: acc).reverse ::: ps
        else r(ps.head, ps.tail, s :: acc)
      }
      r(2, (3 to rg.end).toList, Nil).dropWhile(_ < rg.start)
  }
  def printGoldbachList(r: Range): List[String] = ???
  def printGoldbachListLimited(r: Range, limit: Int): List[String] = ???

}
