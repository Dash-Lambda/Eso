package pdoubleprime

import common.{Interpreter, InterpreterException}

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.util.{Failure, Success, Try}

object PDoublePrime extends Interpreter{
  val name: String = "P''"
  
  def apply(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(progRaw: String): Try[String] = {
    getParms(bools, nums)("log", "debug", "fromPtr", "startHead", "printNull")("initTapeSize", "outputMaxLength", "dbTim") match{
      case Some((log +: debug +: fromPtr +: startHead +: printNull +: _, initTapeSize +: outputMaxLength +: dbTim +: _)) => condition(progRaw) match{
        case (alpha, init, jMap, prog) => eval(initTapeSize, outputMaxLength, dbTim, fromPtr, startHead, printNull, log, debug)(alpha, init, jMap, prog)
      }
      case None => Failure(InterpreterException("Unspecified Configuration Parameters"))
    }
  }
  
  def eval(initTapeSize: Int, outputMaxLength: Int, dbTim: Int,
           fromPtr: Boolean, startHead: Boolean, printNull: Boolean, log: Boolean, debug: Boolean)
          (alpha: Vector[Char], init: Vector[Char], jMap: immutable.HashMap[Int, Int], prog: Vector[Char]): Try[String] = {
    @tailrec
    def pdo(pc: Int, tc: Int, tape: Vector[Int]): String = Try{prog(pc)} match{
      case Success(c) =>
        if(debug){
          println(s"- $c: -${tape.map(alpha(_)).mkString}- [${tape.mkString(" ")}] ($pc, $tc)")
          Thread.sleep(dbTim)
        }
        c match{
          case '(' => pdo(if(tape(tc) == 0) jMap(pc) + 1 else pc + 1, tc, tape)
          case ')' => pdo(if(tape(tc) != 0) jMap(pc) + 1 else pc + 1, tc, tape)
          case 'R' => pdo(pc + 1, tc + 1, tape)
          case 'A' =>
            if(tc > 0) pdo(pc + 1, tc - 1, tape.updated(tc, (tape(tc) + 1)%alpha.size))
            else pdo(pc + 1, tc, 0 +: tape.updated(tc, (tape(tc) + 1)%alpha.size))
        }
      case _ =>
        val trunc = if(fromPtr) tape.drop(tc) else tape
        val chars = (if(printNull) trunc else trunc.filter(_ > 0)).map(alpha(_))
        if(outputMaxLength == -1) chars.mkString
        else if(fromPtr) chars.take(outputMaxLength).mkString
        else chars.takeRight(outputMaxLength).mkString
    }
    
    if(debug) println(
      s"""|Alphabet: ${alpha.map(c => s"'$c'").mkString(", ")}
          |Init: ${init.mkString}
          |Program: ${prog.mkString}""".stripMargin)
    
    Try{
      if(debug) println("- Instruction: -Tape- [Tape Nums] (pc, tc)")
      
      val initTape = init.map(alpha.indexOf(_)).reverse.padTo(initTapeSize, 0).reverse
      val res = pdo(0, if(startHead) math.max(0, initTapeSize - init.size) else initTape.size - 1, initTape)
      
      if(log) println(res)
      res
    }
  }
  
  def condition(progRaw: String): (Vector[Char], Vector[Char], immutable.HashMap[Int, Int], Vector[Char]) = {
    val lines = progRaw.split("(\r\n|\r|\n)")
    val alpha = lines
      .head
      .toVector
    val init = lines
      .tail.head
      .toVector
    val prog = lines
      .tail.tail
      .mkString
      .replaceAllLiterally("L", "r'A")
      .replaceAllLiterally("r'", "r" * (alpha.length - 1))
      .replaceAllLiterally("r", "AR")
      .filter("()RA".contains(_))
      .toVector
    lazy val jMap = mdo(Vector[(Int, Int)](), List[Int](), 0)
    
    @tailrec
    def mdo(ac: Vector[(Int, Int)], stack: List[Int], i: Int): immutable.HashMap[Int, Int] = Try{prog(i)} match{
      case Success(c) => c match{
        case '(' => mdo(ac, i +: stack, i + 1)
        case ')' => mdo(ac :+ (i, stack.head) :+ (stack.head, i), stack.tail, i + 1)
        case _ => mdo(ac, stack, i + 1)
      }
      case _ => mkMap(ac)
    }
    
    (alpha, init, jMap, prog)
  }
}
