package brainfuck

import common.{Config, Generator}

import scala.annotation.tailrec
import scala.util.Try

object BFGen extends Generator{
  val src: String = "BrainFuck"
  val dst: String = "Scala"
  
  def apply(config: Config)(progRaw: String): Try[String] = Try{(config.num("init"), config.num("olen"), config.num("methSize"), config.bool("dyn"), config.bool("indent"))} flatMap{
    case (init, olen, methSize, dyn, ind) =>
      BFOptimize.compOpt(progRaw) map{prog =>
        val gen = genProg(init, olen, dyn, methSize, prog)
        if(ind) indent(gen)
        else gen
      }
  }
  
  def genProg(init: Int, olen: Int, dyn: Boolean, methSize: Int, prog: LazyList[(Char, Either[Int, BlkOp])]): String = {
    def seg(block: Vector[String]): Vector[String] = {
      if (block.sizeIs > methSize - 2) {
        val fnam = block.head.drop(4).takeWhile(_ != '(')
        val lines = block.tail.init
        val groups = lines.grouped(methSize).to(LazyList)
        val funcs = groups
          .zipWithIndex
          .map {
            case (vec, i) =>
              s"""|def ${fnam}s$i(): Unit = {
                  |${vec.mkString("\n")}
                  |}""".stripMargin
          }
        val calls = (0 until groups.length).map { i => s"${fnam}s$i()${if(olen >= 0) "\nif(end) return ()" else ""}" }
        val mf =
          s"""|${block.head}
              |${calls.mkString("\n")}
              |}""".stripMargin
      
        mf +: funcs.toVector
      }else Vector(block.mkString("\n"))
    }
    
    @tailrec
    def cgo(funcs: Vector[String], ac: Vector[String], tmp: Vector[Vector[String]], src: LazyList[(Char, Either[Int, BlkOp])], fnum: Int): Vector[String] = {
      src match{
        case (op, arg) +: ops => op match{
          case '[' | ']' | 'e' => op match{
            case '[' =>
              val call = s"f$fnum()${if(olen >= 0) "\nif(end) return ()" else ""}"
              val sig = s"def f$fnum(): Unit = while(tape(p) != 0){"
              cgo(funcs, Vector(sig), (ac :+ call) +: tmp, ops, fnum + 1)
            case ']' => cgo(seg(ac :+ "}").mkString("\n") +: funcs, tmp.head, tmp.tail, ops, fnum)
            case 'e' => seg(ac :+ "}").mkString("\n") +: funcs
          }
          case _ =>
            val block = arg match{
              case Right(bop) => op match{
                case 'u' | 'a' => s"${if(dyn) s"chkInd(${bop.maxShift})\n" else ""}${bop.opStr}"
                case 'l' => s"${if(dyn) s"chkInd(${bop.maxShift})\n" else ""}${bop.lopStr}"
              }
              case Left(num) => op match{
                case 'm' => s"p ${if (num > 0) "+=" else "-="} ${num.abs}"
                case '/' =>
                  if (num == 1) s"p = tape.indexOf(0, p)"
                  else if (num == -1) s"p = tape.lastIndexOf(0, p)"
                  else if (num > 0) s"while(${if(dyn) "p < len && " else ""}tape(p) != 0){p += $num}"
                  else s"while(${if(dyn) "p < len && " else ""}tape(p) != 0){p -= ${num.abs}}"
                case '[' => "while(tape(p) != 0){"
                case ']' => "}"
                case ',' => "tape(p) = inp.head.toInt\ninp = inp.tail"
                case '.' =>
                  val limStr =
                    s"""|
                        |resLen += 1
                        |if(resLen >= $olen){end = true; return ()}""".stripMargin
                  s"queue.put(Some(Success(tape(p).toChar)))${if(olen >= 0) limStr else ""}"
              }
            }
            val block2 = if(dyn && "m/".contains(op)) s"$block\nchkInd()" else block
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
    
    val methStr = cgo(Vector[String](), Vector("def f0(): Unit = {"), Vector[Vector[String]](), prog, 1).mkString("\n\n")
  
    s"""|import java.util.concurrent.{BlockingQueue, SynchronousQueue}
        |import scala.util.{Try, Success, Failure}
        |
        |new Function2[BlockingQueue[Option[Try[Char]]], Seq[Char], Runnable]{
        |  def apply(queue: BlockingQueue[Option[Try[Char]]], inputs: Seq[Char]): Runnable = Stepper(queue, inputs)
        |
        |  case class Stepper(queue: BlockingQueue[Option[Try[Char]]], inputs: Seq[Char]) extends Runnable{
        |    var tape = Array[Int]()${if(dyn) s"\nvar len = 0" else ""}
        |    var p = 0
        |    var inp = Seq[Char]()${if(olen >= 0) s"\nvar resLen = 0\nvar end = false" else ""}${if(dyn) dynFunc else ""}
        |
        |    def run(): Unit = {
        |      inp = inputs
        |      tape = Array.fill($init)(0)
        |      p = 0
        |      ${if(dyn) s"len = $init" else ""}
        |      try{
        |        f0()
        |        queue.put(None)
        |      }catch{
        |        case e: Throwable => queue.put(Some(Failure(e)))
        |      }
        |    }
        |
        |    $methStr
        |  }
        |}""".stripMargin
  }
}
