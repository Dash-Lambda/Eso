import interpreters.BFFunctional.bfRun
import translators.BrainPuff.{bpTobf, bfTobp}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Tester {
  def main(args: Array[String]): Unit = {
    val prog = "><+-.,[]"
    println(bfTobp(prog))
  }
  
  @tailrec
  def controlLoop: Boolean = {
    val prompt =
      """|Choose a language...
         |  1: Brainfuck
         |  2: BrainPuff
         |
         |  0: Exit
         |> """.stripMargin
    
    grabSel(prompt) match{
      case 0 => true
      case 1 =>
        val prog = grabProg
        println
        bfRun(prog) match{
          case None => println("Error: Program failed")
          case _ =>
        }
        println
        controlLoop
      case 2 =>
        val prog = grabProg
        println
        bfRun(bpTobf(prog)) match{
          case None => println("Error: Program failed")
          case _ =>
        }
        println
        controlLoop
      case n =>
        println(s"Error: $n is not a valid option.\n")
        controlLoop
    }
  }
  
  def grabSel(prompt: String): Int = getInp{print(prompt); scala.io.StdIn.readInt}
  
  def grabProg: String = getInp{
    val nam = scala.io.StdIn.readLine("Name of program text file: ")
    val fin = Source.fromFile(nam)
    val str = fin.getLines.mkString
    fin.close()
    str
  }
  
  @tailrec
  def getInp[T](func: => T): T = Try{func} match{
    case Success(inp) => inp
    case Failure(e) =>
      println(s"Error: $e")
      getInp(func)
  }
}
