package brainfuck

import common.{Generator, InterpreterException}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object BFGenerator extends Generator{
  val src: String = "BrainFuck"
  val dst: String = "Scala"
  def apply(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(progRaw: String): Try[String] = {
    getParms(bools, nums)("log", "debug", "dynamicTapeSize")("outputMaxLength", "initTapeSize", "methodSize") match{
      case Some((log +: debug +: dynamicTapeSize +: _, outputMaxLength +: initTapeSize +: methodSize +: _)) =>
        BFOptimizer.compOpt(progRaw, debug) match{
          case Some((bops, prog)) => Success(generate(initTapeSize, outputMaxLength, methodSize, dynamicTapeSize, log, debug)(bops, prog))
          case None => Failure(InterpreterException(""))
        }
      case None => Failure(InterpreterException("Unspecified Runtime Parameters"))
    }
  }
  
  def generate(initTapeSize: Int, outputMaxLength: Int, methodSize: Int, dynamicTapeSize: Boolean, log: Boolean, debug: Boolean)(bops: Vector[BlkOP], prog: Vector[(Char, Int)]): String = {
    def seg(block: Vector[String]): Vector[String] = {
      if (block.sizeIs > methodSize - 2) {
        val fnam = block.head.drop(4).takeWhile(_ != '(')
        val lines = block.tail.init
        val groups = lines.grouped(methodSize).to(LazyList)
        val funcs = groups
          .zipWithIndex
          .map {
            case (vec, i) =>
              s"""|def ${fnam}s$i(): Unit = {
                  |${vec.mkString("\n")}
                  |}""".stripMargin
          }
        val calls = (0 until groups.length).map { i => s"${fnam}s$i()${if (outputMaxLength != -1) "\nif(end) return" else ""}" }
        val mf =
          s"""|${block.head.init}
              |${calls.mkString("\n")}
              |}""".stripMargin
        
        mf +: funcs.toVector
      }else Vector(block.mkString("\n"))
    }
    
    @tailrec
    def cgo(funcs: List[String], ac: Vector[String], tmp: List[Vector[String]], src: Vector[(Char, Int)], fnum: Int): List[String] = {
      if (debug) {
        val per = 100 - (src.length * 100) / prog.length
        val dots = "." * ((per - 1) / 10)
        print(s"\u001B[100D$per% $dots")
      }
      src match {
        case (op, num) +: ops => op match {
          case '[' | ']' | 'e' => op match {
            case '[' =>
              val call = s"f$fnum()${if (outputMaxLength != -1) "\nif(end) return" else ""}"
              val sig = s"def f$fnum(): Unit = while(tape(p) != 0){"
              cgo(funcs, Vector(sig), (ac :+ call) +: tmp, ops, fnum + 1)
            case ']' => cgo(seg(ac :+ "}").mkString("\n") +: funcs, tmp.head, tmp.tail, ops, fnum)
            case 'e' => cgo(seg(ac :+ "}").mkString("\n") +: funcs, Vector[String](), tmp, ops, fnum)
          }
          case _ =>
            val block = op match {
              case 'u' | 'a' => bops(num).opStr
              case 'l' => bops(num).lopStr
              case 'm' => s"p ${if (num > 0) "+=" else "-="} ${num.abs}"
              case '/' =>
                if (num == 1) s"p = tape.indexOf(0, p)"
                else if (num == -1) s"p = tape.lastIndexOf(0, p)"
                else if (num > 0) s"while(tape(p) != 0){p += $num}"
                else s"while(tape(p) != 0){p -= ${num.abs}}"
              case '[' => "while(tape(p) != 0){"
              case ']' => "}"
              case ',' => "tape(p) = getInp"
              case '.' =>
                val logger = if (log) "\nprint(tape(p).toChar)" else ""
                val limiter = if (outputMaxLength != -1) s"\nif(res.sizeIs >= $outputMaxLength){end = true; return}" else ""
                s"res += tape(p).toChar$logger$limiter"
            }
            cgo(funcs, ac :+ block, tmp, ops, fnum)
        }
        case _ => funcs
      }
    }
    
    if (debug) println("Generating:")
    val methStr = cgo(List[String](), Vector("def f0(): Unit = {"), List[Vector[String]](), prog, 1).mkString("\n")
    if (debug) println
    s"""|new Function0[String]{
        |private val tape = Array.fill($initTapeSize)(0)
        |private var p = 0
        |val res = new StringBuilder()
        |var inLog = Vector[Int]()${if (outputMaxLength != -1) "\nvar end = false" else ""}
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
        |$methStr}""".stripMargin
  }
}
