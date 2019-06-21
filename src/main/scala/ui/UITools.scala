package ui

import java.io.{File, FileWriter, PrintWriter}

import translators.BFTranslator

import scala.annotation.tailrec
import scala.io.{Source, StdIn}
import scala.util.{Failure, Success, Try}



object UITools {
  def saveBFLang(fnam: String)(trans: BFTranslator): Unit = {
    val oFile = new FileWriter(fnam, true)
    val pairString = trans.kvPairs.map(p => s"${p._1}=${p._2}").mkString("\n")
    val logString = s"\nname=${trans.name}\n$pairString"
    oFile.write(logString)
    oFile.close()
  }
  
  def transProg(trans: String => String): Boolean = {
    val prog = grabProg("Name of source program text file: ")
    val dest = grabStr("Name of destination file: ")
    Try{
      val oFile = new PrintWriter(new File(dest))
      oFile.print(trans(prog))
      oFile.close()
    } match{
      case Success(_) =>
        println("Program saved successfully.\n")
        true
      case Failure(_) =>
        println("Error: Could not translate and/or save program.\n")
        false
    }
  }
  
  def grabAndRunProg(interp: String => Option[String]): Boolean = {
    val prog = grabProg("Name of program text file: ")
    println("Running...\n")
    interp(prog) match{
      case Some(_) =>
        println("\nProgram completed successfully\n")
        true
      case None =>
        println("\nError: Program Failed\n")
        false
    }
  }
  
  def grabSel(prompt: String): Int = getInp{print(prompt); StdIn.readInt}
  def grabStr(prompt: String): String = getInp{print(prompt); StdIn.readLine}
  def grabProg(prompt: String): String = getInp{
    val nam = grabStr(prompt)
    val fin = Source.fromFile(nam)
    val str = fin.mkString
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
