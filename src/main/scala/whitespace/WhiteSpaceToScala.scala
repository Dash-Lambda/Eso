package whitespace

import common.{Config, Transpiler}
import WSCommon._
import spire.math.SafeLong

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.Try

object WhiteSpaceToScala extends Transpiler{
  val src: String = "WhiteSpace"
  val dst: String = "Scala"
  
  def apply(config: Config)(progRaw: String): Try[String] = Try{condition(progRaw)} map{prog =>
    val progStr = genProg(prog, getCalls(prog))
    if(config.bool("indent")) indent(progStr)
    else progStr
  }
  
  def genProg(prog: Vector[(String, SafeLong)], calls: immutable.HashMap[SafeLong, Int]): String = {
    @tailrec
    def gpo(ac: Vector[String], src: Vector[(String, SafeLong)], sub: Boolean = true, count: Int = 1): String = src match{
      case (s, n) +: ops =>
        val str = s match{
          case "push" => s"stack.push($n)"
          case "dup" => "dup()"
          case "swap" => "swap()"
          case "discard" => "stack.pop"
          case "add" => "add()"
          case "subt" => "subt()"
          case "mult" => "mult()"
          case "intDiv" => "intDiv()"
          case "mod" => "mod()"
          case "store" => "store()"
          case "get" => "get()"
          case "label" =>
            if(sub) s"f$n()\n}\ndef f$n(): Unit = {"
            else s"def f$n(): Unit = {"
          case "call" => s"f$n()\nif(endProg) return"
          case "jump" => s"f$n()\nreturn"
          case "jumpZero" => s"if(stack.pop == 0) {f$n(); return}"
          case "jumpNeg" => s"if(stack.pop < 0) {f$n(); return}"
          case "return" =>
            if(sub) "}"
            else ""
          case "readChar" => "stack.push(getChar)"
          case "readNum" => "stack.push(getNum)"
          case "outChar" => "print(stack.pop.toChar)"
          case "outNum" => "print(stack.pop)"
          case "endProg" =>
            if(sub) "endProg = true\n}"
            else "endProg = true"
        }
        
        val nSub = s match{
          case "label" => true
          case "return" => false
          case _ => sub
        }
        gpo(ac :+ str, ops, nSub, count + 1)
      case _ => ac.mkString("\n")
    }
    
    val dupStr = "def dup(): Unit = {val tmp = stack.pop; stack.push(tmp); stack.push(tmp)}"
    val swapStr = "def swap(): Unit = {val a = stack.pop; val b = stack.pop; stack.push(a); stack.push(b)}"
    val addStr = "def add(): Unit = {val a = stack.pop; val b = stack.pop; stack.push(a + b)}"
    val subtStr = "def subt(): Unit = {val a = stack.pop; val b = stack.pop; stack.push(a - b)}"
    val multStr = "def mult(): Unit = {val a = stack.pop; val b = stack.pop; stack.push(a * b)}"
    val intDivStr = "def intDiv(): Unit = {val a = stack.pop; val b = stack.pop; stack.push(a / b)}"
    val modStr = "def mod(): Unit = {val a = stack.pop; val b = stack.pop; stack.push(a % b)}"
    val storeStr = "def store(): Unit = {val n = stack.pop; val addr = stack.pop; heap += ((addr, n))}"
    val getStr = "def get(): Unit = {val addr = stack.pop; stack.push(heap(addr))}"
    
    val progStr = gpo(Vector(), prog)
    s"""|import scala.collection._
        |import spire.math.SafeLong
        |import spire.implicits._
        |
        |var inp = LazyList.continually(scala.io.StdIn.readLine()).flatMap(_ + "\\n")
        |var heap = mutable.HashMap[SafeLong, SafeLong]()
        |var stack = mutable.Stack[SafeLong]()
        |var endProg = false
        |
        |def getChar: Char = {val c = inp.head; inp = inp.tail; c}
        |def getNum: SafeLong = {val nstr = inp.takeWhile(_.isDigit).mkString; inp = inp.dropWhile(_.isDigit); SafeLong(BigInt(nstr))}
        |$dupStr
        |$swapStr
        |$addStr
        |$subtStr
        |$multStr
        |$intDivStr
        |$modStr
        |$storeStr
        |$getStr
        |
        |def fInit(): Unit = {
        |$progStr
        |
        |fInit()""".stripMargin
  }
}
