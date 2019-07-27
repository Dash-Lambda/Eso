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
          s"""|${block.head}
              |${calls.mkString("\n")}
              |}""".stripMargin
        
        mf +: funcs.toVector
      }else Vector(block.mkString("\n"))
    }
    
    @tailrec
    def cgo(funcs: List[String], ac: Vector[String], tmp: List[Vector[String]], src: Vector[(Char, Int)], fnum: Int): List[String] = {
      if (debug) print(percBar(src.length, prog.length))
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
              case 'u' | 'a' => s"${if(dynamicTapeSize) s"chkInd(${bops(num).maxShift})\n" else ""}${bops(num).opStr(dynamicTapeSize)}"
              case 'l' => s"${if(dynamicTapeSize) s"chkInd(${bops(num).maxShift})\n" else ""}${bops(num).lopStr(dynamicTapeSize)}"
              case 'm' => s"p ${if (num > 0) "+=" else "-="} ${num.abs}"
              case '/' =>
                if (num == 1) s"p = tape.indexOf(0, p)"
                else if (num == -1) s"p = tape.lastIndexOf(0, p)"
                else if (num > 0) s"while(${if(dynamicTapeSize) "p < len && " else ""}tape(p) != 0){p += $num}"
                else s"while(${if(dynamicTapeSize) "p < len && " else ""}tape(p) != 0){p -= ${num.abs}}"
              case '[' => "while(tape(p) != 0){"
              case ']' => "}"
              case ',' => "tape(p) = getInp"
              case '.' =>
                val logger = if (log) "\nprint(tape(p).toChar)" else ""
                val limiter = if (outputMaxLength != -1) s"\nif(res.sizeIs >= $outputMaxLength){end = true; return}" else ""
                s"res += tape(p).toChar$logger$limiter"
            }
            val block2 = if(dynamicTapeSize && "m/".contains(op)) s"$block\nchkInd()" else block
            cgo(funcs, ac :+ block2, tmp, ops, fnum)
        }
        case _ => funcs
      }
    }
    
    val dynFunc: String =
      s"""|
          |
          |def chkInd(shift: Int = 0): Unit = {
          |  if(p == -1){p = len; len += 1; tape = tape.padTo(len, 0)}
          |  else if(p + shift >= len){len = p + shift + 1; tape = tape.padTo(len, 0)}
          |}""".stripMargin
    
    if (debug) println("Generating:")
    val methStr = cgo(List[String](), Vector("def f0(): Unit = {"), List[Vector[String]](), prog, 1).mkString("\n")
    if (debug) println
    s"""|new Function0[String]{
        |var tape = Array.fill($initTapeSize)(0)${if(dynamicTapeSize) s"\nvar len = $initTapeSize" else ""}
        |var p = 0
        |val res = new StringBuilder()
        |var inLog = Vector[Int]()${if (outputMaxLength != -1) "\nvar end = false" else ""}${if(dynamicTapeSize) dynFunc else ""}
        |
        |def getInp: Int = {
        |  if(inLog.nonEmpty){
        |    val ret = inLog.head
        |    inLog = inLog.tail
        |    ret
        |  }else{
        |    val inp = scala.io.StdIn.readLine.toVector.map(_.toInt) :+ 10
        |    inLog = inp.tail
        |    inp.head
        |  }
        |}
        |
        |def apply: String = {
        |  f0()
        |  val out = res.result
        |  res.clear()
        |  out
        |}
        |
        |$methStr}""".stripMargin
  }
}
