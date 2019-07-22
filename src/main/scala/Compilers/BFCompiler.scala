package Compilers

import interpreters.InterpreterException

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

case class CompOp(ops: Vector[(Int, String)], shift: Int){
  def sumat(i: Int): Int = ops.filter(_._1 == i).map{
    case (_, str) =>
      if(str.startsWith("+")) str.drop(3).toInt
      else if(str.startsWith("-")) -str.drop(3).toInt
      else 0
  }.foldLeft(0){case (ac, n) => if(n == 0) 0 else ac + n}
  
  def canLoop: Boolean = (sumat(0) == -1) && ops.map(_._2).forall(str => str.startsWith("+") || str.startsWith("-")) && shift == 0
  def maxShift: Int = ops.map(_._1).max
}

object BFCompiler extends Compiler{
  val src: String = "BrainFuck"
  val dst: String = "Scala"
  
  def apply(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(progRaw: String): Try[String] = {
    getParms(bools, nums)("log", "debug", "dynamicTapeSize")("outputMaxLength", "initTapeSize", "methodSize") match{
      case Some((log +: debug +: dynamicTapeSize +: _, outputMaxLength +: initTapeSize +: methodSize +: _)) =>
        compOpt(progRaw, debug) match{
          case Success((bops, prog)) => Success(compile(initTapeSize, outputMaxLength, methodSize, dynamicTapeSize, log, debug)(bops, prog))
          case Failure(e) => Failure(e)
        }
      case None => Failure(InterpreterException("Unspecified Runtime Parameters"))
    }
  }
  
  def compile(initTapeSize: Int, outputMaxLength: Int, methodSize: Int, dynamicTapeSize: Boolean, log: Boolean, debug: Boolean)(bops: Vector[CompOp], prog: Vector[(Char, Int)]): String = {
    def seg(block: Vector[String]): Vector[String] = {
      if(block.sizeIs > methodSize - 2){
        val fnam = block.head.drop(4).takeWhile(_ != '(')
        val lines = block.tail.init
        val groups = lines.grouped(methodSize).to(LazyList)
        val funcs = groups
          .zipWithIndex
          .map{
            case (vec, i) =>
              s"""|def ${fnam}s$i(): Unit = {
                  |${vec.mkString}
                  |}""".stripMargin}
        val calls = (0 until groups.length).map{i => s"${fnam}s$i()${if(outputMaxLength != -1) "\nif(end) return" else ""}"}
        val mf =
          s"""|${block.head.init}
              |${calls.mkString("\n")}
              |}""".stripMargin
        
        mf +: funcs.toVector
      }else{
        Vector(block.mkString)
      }
    }
    
    @tailrec
    def cgo(funcs: List[String], ac: Vector[String], tmp: List[Vector[String]], src: Vector[(Char, Int)], fnum: Int): List[String] = {
      if(debug){
        val per = 100 - (src.length*100)/prog.length
        val dots = "."*((per - 1)/10)
        print(s"\u001B[100D$per% $dots")
      }
      src match {
        case (op, num) +: ops => op match {
          case ']' | '[' | 'e' => op match {
            case '[' =>
              val call = s"f$fnum()\n${if(outputMaxLength != -1) "if(end) return\n" else ""}"
              val sig = s"def f$fnum(): Unit = while(tape(p) != 0){\n"
              cgo(funcs, Vector(sig), (ac :+ call) +: tmp, ops, fnum + 1)
            case ']' => cgo(seg(ac :+ "}").mkString("\n") +: funcs, tmp.head, tmp.tail, ops, fnum)
            case 'e' => cgo(seg(ac :+ "}").mkString("\n") +: funcs, Vector[String](), tmp, ops, fnum)
          }
          case _ =>
            val block = op match {
              case 'u' =>
                val ups = bops(num).ops.map { case (ind, opstr) =>
                  if (ind != 0) s"tape(p + $ind) $opstr"
                  else s"tape(p) $opstr"
                }.mkString("\n")
                s"$ups${if (bops(num).shift != 0) s"\np += ${bops(num).shift}" else ""}"
              case 'l' =>
                val ups = bops(num).ops.map { case (ind, opstr) =>
                  if (!opstr.startsWith("=") && ind != 0) s"tape(p + $ind) $opstr*tmp"
                  else if (ind != 0) s"tape(p + $ind) $opstr"
                  else s"tape(p) = 0"
                }.mkString("\n")
                s"""|if(tape(p) != 0){
                    |val tmp = tape(p)
                    |$ups
                    |}""".stripMargin
              case 'm' => s"p ${if (num >= 0) "+" else "-"}= ${num.abs}"
              case '/' =>
                if (num.abs == 1) s"p = tape.${if (num == 1) "indexOf" else "lastIndexOf"}(0, p)"
                else s"while(tape(p) != 0){ p ${if (num >= 0) "+" else "-"}= ${num.abs} }"
              case '[' => "while(tape(p) != 0){"
              case ']' => "}"
              case '_' => "tape(p) = 0"
              case ',' => "tape(p) = getInp"
              case '.' => s"""res += tape(p).toChar${if(log) "\nprint(tape(p).toChar)" else ""}${if(outputMaxLength != -1) s"\nif(res.sizeIs >= $outputMaxLength){end = true; return}" else ""}""".stripMargin
            }
            val nxt =
              s"""|$block
                  |""".stripMargin
            cgo(funcs, ac :+ nxt, tmp, ops, fnum)
        }
        case _ => funcs
      }
    }
    
    if(debug) println("Composing:")
    val funcs = cgo(List[String](), Vector("def f0(): Unit = {\n"), List[Vector[String]](), prog, 1).mkString("\n")
    if(debug) println
    s"""|new Function0[String]{
        |private val tape = Array.fill($initTapeSize)(0)
        |private var p = 0
        |val res = new StringBuilder()
        |var inLog = Vector[Int]()${if(outputMaxLength != -1) "\nvar end = false" else ""}
        |
        |def getInp: Int = {
        |if(inLog.nonEmpty){
        |val ret = inLog.head
        |inLog = inLog.tail
        |ret
        |}else{
        |val inp = scala.io.StdIn.readLine.toVector.map(_.toInt) :+ 10
        |inLog = inp.tail
        |inp.head
        |}}
        |
        |def apply: String = {
        |f0()
        |res.result
        |}
        |
        |$funcs}""".stripMargin
  }
  
  def compOpt(progRaw: String, debug: Boolean): Try[(Vector[CompOp], Vector[(Char, Int)])] = {
    def optBase(progSrc: String): Vector[(Char, Int)] = {
      val ops = Vector[Char]('>', '<', '+', '-')
      val nonOps = Vector[Char]('[', ']', ',', '.', '_')
      val conditioned = progRaw.filter("><][+-,.".contains(_)).replaceAll("""\[(\+|-)\]""", "_")
      
      @tailrec
      def bHelper(ac: Vector[(Char, Int)], tag: Char, count: Int, src: String): Vector[(Char, Int)] = {
        if(debug){
          val per = 100 - (src.length*100)/conditioned.length
          val dots = "."*((per - 1)/10)
          print(s"\u001B[100D$per% $dots")
        }
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
      
      if(debug) println("Base optimization:")
      val res = bHelper(Vector[(Char, Int)](), ' ', 0, conditioned) :+ ('e', 0)
      if(debug) println
      res
    }
    
    def optBulk(progSrc: Vector[(Char, Int)]): (Vector[CompOp], Vector[(Char, Int)]) = {
      @tailrec
      def uHelper(ac: Vector[(Char, Int)], mac: Vector[CompOp], vac: Vector[(Int, String)], shift: Int, ind: Int, src: Vector[(Char, Int)]): (Vector[CompOp], Vector[(Char, Int)]) = {
        if(debug){
          val per = 100 - (src.length*100)/progSrc.length
          val dots = "."*((per - 1)/10)
          print(s"\u001B[100D$per% $dots")
        }
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
      
      if(debug) println("Bulk Optimization:")
      val res = uHelper(Vector[(Char, Int)](), Vector[CompOp](), Vector[(Int, String)](), 0, 0, progSrc)
      if(debug) println
      res
    }
    
    def optLoop(progSrc: Vector[(Char, Int)], hmap: Vector[CompOp]): Vector[(Char, Int)] = {
      @tailrec
      def lHelper(ac: Vector[(Char, Int)], src: Vector[(Char, Int)]): Vector[(Char, Int)] = {
        if(debug){
          val per = 100 - (src.length*100)/progSrc.length
          val dots = "."*((per - 1)/10)
          print(s"\u001B[100D$per% $dots")
        }
        src match{
          case ('[', _) +: ('u', num) +: (']', _) +: tail if hmap(num).canLoop => lHelper(ac :+ ('l', num), tail)
          case ('[', _) +: ('m', stp) +: (']', _) +: tail => lHelper(ac :+ ('/', stp), tail)
          case p +: ps => lHelper(ac :+ p, ps)
          case _ => ac
        }
      }
      
      if(debug) println("Simple Loop Optimization:")
      val res = lHelper(Vector[(Char, Int)](), progSrc)
      if(debug) println
      res
    }
    
    val pass1 = optBase(progRaw)
    val (hmap, pass2) = optBulk(pass1)
    val pass3 = optLoop(pass2, hmap)
    
    if(pass3.count(_._1 == '[') == pass3.count(_._1 == ']')) Success(hmap, pass3)
    else Failure(InterpreterException("Bracket Mismatch"))
  }
}
