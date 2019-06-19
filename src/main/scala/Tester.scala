import interpreters.BFFunctional.bfRun

import util.control.Breaks._

import scala.io.Source
import scala.util.{Failure, Success, Try}

object Tester {
  def main(args: Array[String]): Unit = {
    while(true){
      val prog = getProg(scala.io.StdIn.readLine("Name of text file containing program: "))
      prog match{
        case Success(str) => bfRun(str)
        case Failure(e) => println(s"Error: $e")
      }
      if(prog.isSuccess) break
    }
  }
  
  def getProg(nam: String): Try[String] = Try{
    val fin = Source.fromFile(nam)
    val str = fin.getLines.mkString
    fin.close()
    str
  }
}
