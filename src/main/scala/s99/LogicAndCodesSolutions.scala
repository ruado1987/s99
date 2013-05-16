package s99

import Solutions.???

trait LogicAndCodesSolutions { outer =>

  implicit def extendBoolean(a: Boolean): ExtendedBoolean = ExtendedBoolean(a)
  case class ExtendedBoolean(a: Boolean) {
    def and (b: =>Boolean): Boolean = outer.and (a, b)
    def or  (b: =>Boolean): Boolean = outer.or  (a, b) 
    def nand(b: =>Boolean): Boolean = outer.nand(a, b)
    def nor (b: =>Boolean): Boolean = outer.nor (a, b)
    def xor (b: =>Boolean): Boolean = outer.xor (a, b)
    def impl(b: =>Boolean): Boolean = outer.impl(a, b)
    def equ (b: =>Boolean): Boolean = outer.equ (a, b)

  }
  
  def and(a: Boolean,  b: =>Boolean): Boolean = if (a) b else false
  def or(a: Boolean,   b: =>Boolean): Boolean = if (a) true else b
  def nand(a: Boolean,  b: =>Boolean): Boolean = if (a) !b else true
  def nor(a: Boolean,  b: =>Boolean): Boolean = if (!a && !b) true else false
  def xor(a: Boolean,  b: =>Boolean): Boolean = if(a) !b else b
  def impl(a: Boolean,  b: =>Boolean): Boolean = if(a && !b) false else true
  def equ(a: Boolean,  b: =>Boolean): Boolean = or(and(a, b), and(not(a), not(b))) 
  def not(a: Boolean) = !a
  
  def table2(f: (Boolean, Boolean) => Boolean): String = {
      Seq("A     B     result",
        "true  " + "true  " + f(true, true).toString.padTo(6, " ").mkString,
        "true  " + "false " + f(true, false).toString.padTo(6, " ").mkString,
        "false " + "true  " + f(false, true).toString.padTo(6, " ").mkString,
        "false " + "false " + f(false, false).toString.padTo(6, " ").mkString
      ).mkString("\n")
  }

  def gray(n: Int): List[String] = ???
  def huffman(list: List[(String,  Int)]): List[(String, String)] = ???

}
