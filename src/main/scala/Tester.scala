import java.io.{File, PrintWriter}

import interpreters.BFFunctional.bfRun
import translators.{BrainPuff, Ook}
import ui.UITools._

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Tester {
  def main(args: Array[String]): Unit = controlLoop()
  
  @tailrec
  def controlLoop(): Unit = {
    val prompt =
      """|Choose an action...
         |  1: Run BrainFuck Program
         |  2: Run BrainPuff Program
         |  3: Run Ook Program
         |
         |  4: Translate BrainFuck => BrainPuff
         |  5: Translate BrainPuff => BrainFuck
         |  6: Translate BrainFuck => Ook
         |  7: translate Ook => BrainFuck
         |
         |  8: Testing Shortcut
         |
         |  0: Exit
         |> """.stripMargin
    
    grabSel(prompt) match{
      case 0 => println("Exiting...")
      case 1 => runProg(bfRun)
        controlLoop()
      case 2 => runProg(prog => bfRun(BrainPuff(prog)))
        controlLoop()
      case 3 => runProg(prog => bfRun(Ook(prog)))
        controlLoop()
      case 4 => transProg(BrainPuff.unapply)
        controlLoop()
      case 5 => transProg(BrainPuff(_))
        controlLoop()
      case 6 => transProg(Ook.unapply)
        controlLoop()
      case 7 => transProg(Ook(_))
        controlLoop()
      case 8 =>
        val bhw = Source.fromFile("bfHelloWorld.txt")
        val ohw = Source.fromFile("OokHW.txt")
        
        val bprog = bhw.mkString
        val oprog = ohw.mkString
        
        bhw.close()
        ohw.close()
        
        val boprog = Ook.unapply(bprog)
        val obprog = Ook(oprog)
        
        println(
          s"""|BrainFuck Source:
              |$bprog
              |
              |Ook Source:
              |$oprog
              |
              |BrainFuck > Ook:
              |$boprog
              |
              |Ook > BrainFuck
              |$obprog""".stripMargin)
    }
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
  
  def runProg(interp: String => Option[String]): Boolean = {
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
}
