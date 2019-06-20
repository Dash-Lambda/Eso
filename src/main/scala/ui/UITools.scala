package ui

import scala.annotation.tailrec
import scala.io.{Source, StdIn}
import scala.util.{Failure, Success, Try}

object UITools {
  final def grabSel(prompt: String): Int = getInp{print(prompt); StdIn.readInt}
  final def grabStr(prompt: String): String = getInp{print(prompt); StdIn.readLine}
  final def grabProg(prompt: String): String = getInp{
    val nam = grabStr(prompt)
    val fin = Source.fromFile(nam)
    val str = fin.mkString
    fin.close()
    str
  }
  
  @tailrec
  final def getInp[T](func: => T): T = Try{func} match{
    case Success(inp) => inp
    case Failure(e) =>
      println(s"Error: $e")
      getInp(func)
  }
}
