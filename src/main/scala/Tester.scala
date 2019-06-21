import interpreters.BFFunctional.bfRun
import translators.{BrainPuff, Ook}
import ui.UITools._

import scala.annotation.tailrec

object Tester {
  def main(args: Array[String]): Unit = controlLoop()
  
  @tailrec
  def controlLoop(): Unit = {
    val prompt =
      """|Choose an action...
         |  1: Run BrainFuck Program
         |  2: Run FlufflePuff Program
         |  3: Run Ook Program
         |
         |  4: Translate BrainFuck => FlufflePuff
         |  5: Translate FlufflePuff => BrainFuck
         |  6: Translate BrainFuck => Ook
         |  7: translate Ook => BrainFuck
         |
         |  0: Exit
         |> """.stripMargin
    
    grabSel(prompt) match{
      case 0 => println("Exiting...")
      case 1 => grabAndRunProg(bfRun)
        controlLoop()
      case 2 => grabAndRunProg(prog => bfRun(BrainPuff(prog)))
        controlLoop()
      case 3 => grabAndRunProg(prog => bfRun(Ook(prog)))
        controlLoop()
      case 4 => transProg(BrainPuff.unapply)
        controlLoop()
      case 5 => transProg(BrainPuff(_))
        controlLoop()
      case 6 => transProg(Ook.unapply)
        controlLoop()
      case 7 => transProg(Ook(_))
        controlLoop()
      case n => println(s"$n is not a valid command.\n")
        controlLoop()
    }
  }
}
