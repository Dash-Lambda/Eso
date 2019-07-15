package Compilers

import interpreters.InterpreterException

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

case class CompOp(ops: Vector[(Int, String)], shift: Int){
  def sumat(i: Int): Int = ops.filter(_._1 == i).map{
    case (_, str) =>
      if(str.startsWith("+")) str.drop(3).toInt
      else if(str.startsWith("-")) -str.drop(3).toInt
      else 0
  }.foldLeft(0){case (ac, n) => if(n == 0) 0 else ac + n}
  
  def canLoop: Boolean = (sumat(0) == -1) && ops.map(_._2).forall(str => str.startsWith("+") || str.startsWith("-"))
  def maxShift: Int = ops.map(_._1).max
}

object BFCompiler {
  def apply(log: Boolean)(progRaw: String): Try[String] = apply(40000, -1, dynamicTapeSize = false, log = false, debug = false)(progRaw)
  def apply(initTapeSize: Int, outputMaxLength: Int, dynamicTapeSize: Boolean, log: Boolean, debug: Boolean)(progRaw: String): Try[String] = {
    compOpt(progRaw, debug) match{
      case Success((bops, prog)) => Success(compile(initTapeSize, outputMaxLength, dynamicTapeSize, log, debug)(bops, prog))
      case Failure(e) => Failure(e)
    }
  }
  
  def compile(initTapeSize: Int, outputMaxLength: Int, dynamicTapeSize: Boolean, log: Boolean, debug: Boolean)(bops: Vector[CompOp], prog: Vector[(Char, Int)]): String = {
    @tailrec
    def cgo(ac: String, src: Vector[(Char, Int)]): String = src match{
      case (op, num) +: ops =>
        val block = op match{
          case 'u' =>
            val ups = bops(num).ops.map{case (ind, opstr) =>
              if(ind != 0) s"tape(p + $ind) $opstr"
              else s"tape(p) $opstr"
            }.mkString("\n")
            s"$ups${if(bops(num).shift != 0) s"\np += ${bops(num).shift}" else ""}"
          case 'l' =>
            val ups = bops(num).ops.map { case (ind, opstr) =>
              if (!opstr.startsWith("=") && ind != 0) s"tape(p + $ind) $opstr*tmp"
              else if(ind != 0) s"tape(p + $ind) $opstr"
              else s"tape(p) = 0"
            }.mkString("\n")
            s"""|if(tape(p) != 0){
                |val tmp = tape(p)
                |$ups
                |}""".stripMargin
          case 'm' => s"p ${if(num >= 0) "+" else "-"}= ${num.abs}"
          case '/' =>
            if(num.abs == 1) s"p = tape.${if(num == 1) "indexOf" else "lastIndexOf"}(0, p)"
            else s"while(tape(p) != 0){ p ${if(num >= 0) "+" else "-"}= ${num.abs} }"
          case '[' => "while(tape(p) != 0){"
          case ']' => "}"
          case '_' => "tape(p) = 0"
          case ',' => "tape(p) = scala.io.StdIn.readInt"
          case '.' =>
            if(log && outputMaxLength == -1) "print(tape(p).toChar)"
            else if(log)
              s"""|print(tape(p).toChar)
                  |length += 1
                  |if(length >= $outputMaxLength) return "Success"""".stripMargin
            else if(outputMaxLength == -1) "res += tape(p).toChar"
            else
              s"""|res += tape(p).toChar
                  |if(res.sizeIs >= $outputMaxLength) return res.result""".stripMargin
          case 'e' =>
            if(log) """"Success""""
            else "res.result"
        }
        val nxt =
          s"""|$block
              |""".stripMargin
        cgo(ac ++ nxt, ops)
      case _ => ac
    }
    
    s"""|new Function0[String]{
        |def apply: String = {
        |val tape = Array.fill($initTapeSize)(0)
        |var p = 0
        |${if(outputMaxLength != -1 && log) "var length = 0\n" else if(!log) "val res = new StringBuilder()\n" else ""}
        |${cgo("", prog)}
        |}}""".stripMargin
  }
  
  def compOpt(progRaw: String, debug: Boolean): Try[(Vector[CompOp], Vector[(Char, Int)])] = {
    def optBase(progSrc: String): Vector[(Char, Int)] = {
      val ops = Vector[Char]('>', '<', '+', '-', '.')
      val nonOps = Vector[Char]('[', ']', ',', '_')
      
      @tailrec
      def bHelper(ac: Vector[(Char, Int)], tag: Char, count: Int, src: String): Vector[(Char, Int)] = {
        if(debug) println(
          s"""|Source: $src
              |Tag: $tag
              |Count: $count
              |Opt: ${ac.mkString(" | ")}
              |""".stripMargin)
        
        src.headOption match{
          case Some(c) =>
            if((tag == ' ') && ops.contains(c)) bHelper(ac, c, 1, src.tail)
            else if((tag == ' ') && nonOps.contains(c)) bHelper(ac :+ (c, 1), ' ', 0, src.tail)
            else if((tag != ' ') && (c == tag)) bHelper(ac, tag, count + 1, src.tail)
            else if((tag != ' ') && (c != tag) && ops.contains(c)) bHelper(ac :+ (tag, count), c, 1, src.tail)
            else bHelper(ac :+ (tag, count) :+ (c, 1), ' ', 0, src.tail)
          case None =>
            if(tag == ' ') ac
            else ac :+ (tag, count)
        }
      }
      
      val conditioned = progSrc.filter("><][+-,.".contains(_))
      val conditioned2 = conditioned.replaceAllLiterally("[-]", "_")
      bHelper(Vector[(Char, Int)](), ' ', 0, conditioned2) :+ ('e', 0)
    }
    
    def optBulk(progSrc: Vector[(Char, Int)]): (Vector[CompOp], Vector[(Char, Int)]) = {
      @tailrec
      def uHelper(ac: Vector[(Char, Int)], mac: Vector[CompOp], vac: Vector[(Int, String)], shift: Int, ind: Int, src: Vector[(Char, Int)]): (Vector[CompOp], Vector[(Char, Int)]) = {
        if(debug) println(
          s"""|Source: ${src.mkString(" | ")}
              |CompOps: ${vac.mkString(" | ")}
              |Shift: $shift
              |Addr: $ind
              |Opt: ${ac.mkString(" | ")}
              |""".stripMargin)
        
        src match{
          case ('>', n) +: tail => uHelper(ac, mac, vac, shift + n, ind, tail)
          case ('<', n) +: tail => uHelper(ac, mac, vac, shift - n, ind, tail)
          case ('+', n) +: tail => uHelper(ac, mac, vac :+ (shift, s"+= $n"), shift, ind, tail)
          case ('-', n) +: tail => uHelper(ac, mac, vac :+ (shift, s"-= $n"), shift, ind, tail)
          case ('_', _) +: tail => uHelper(ac, mac, vac :+ (shift, "= 0"), shift, ind, tail)
          case p +: ps =>
            if(vac.nonEmpty) uHelper(ac :+ ('u', ind) :+ p, mac :+ CompOp(vac.map(op => (op._1, op._2)), shift), Vector[(Int, String)](), 0, ind + 1, ps)
            else if(vac.isEmpty && shift != 0) uHelper(ac :+ ('m', shift) :+ p, mac, Vector[(Int, String)](), 0, ind, ps)
            else uHelper(ac :+ p, mac, vac, shift, ind, ps)
          case _ => (mac, ac)
        }
      }
      
      uHelper(Vector[(Char, Int)](), Vector[CompOp](), Vector[(Int, String)](), 0, 0, progSrc)
    }
    
    def optLoop(progSrc: Vector[(Char, Int)], hmap: Vector[CompOp]): Vector[(Char, Int)] = {
      @tailrec
      def lHelper(ac: Vector[(Char, Int)], src: Vector[(Char, Int)]): Vector[(Char, Int)] = {
        if(debug) println(
          s"""|Source: ${src.mkString(" | ")}
              |Opt: ${ac.mkString(" | ")}
              |""".stripMargin)
        
        src match{
          case ('[', _) +: ('u', num) +: (']', _) +: tail if hmap(num).canLoop => lHelper(ac :+ ('l', num), tail)
          case ('[', _) +: ('m', stp) +: (']', _) +: tail => lHelper(ac :+ ('/', stp), tail)
          case p +: ps => lHelper(ac :+ p, ps)
          case _ => ac
        }
      }
      
      lHelper(Vector[(Char, Int)](), progSrc)
    }
    
    def counts(src: Vector[(Char, Int)]): String = s"[${src.count(_._1 == '[')}, ${src.count(_._1 == ']')}]"
    
    if(debug) println("Base optimization...\n")
    val pass1 = optBase(progRaw)
    if(debug) println(s"Bulk Optimization...\n")
    val (hmap, pass2) = optBulk(pass1)
    if(debug) println(s"Simple Loop Optimization...\n")
    val pass3 = optLoop(pass2, hmap)
    if(debug) println(
      s"""|
          |Base:  ${progRaw.filter("><+-[],.".contains(_))}
          |Pass1: ${pass1.map(_._1).mkString}
          |Pass2: ${pass2.map(_._1).mkString}
          |Pass3: ${pass3.map(_._1).mkString}
          |Bracket Sums: [${progRaw.count(_ == '[')}, ${progRaw.count(_ == ']')}] -> ${counts(pass1)} -> ${counts(pass2)} -> ${counts(pass3)}
          |""".stripMargin)
    
    if(pass3.count(_._1 == '[') == pass3.count(_._1 == ']')) Success(hmap, pass3)
    else Failure(InterpreterException("Bracket Mismatch"))
  }
}
