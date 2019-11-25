package ui

import common.{Config, EsoObj, Interpreter, Translator, Transpiler}
import ui.ConsoleUtil._

import scala.collection.mutable
import scala.io.StdIn

object Console extends EsoObj{
  val pointer: String = EsoDefaults.defPointer
  val welcome: String = EsoDefaults.defWelcome
  
  private val bools: mutable.HashMap[String, Boolean] = mutable.HashMap[String, Boolean]()
  private val nums: mutable.HashMap[String, Int] = mutable.HashMap[String, Int]()
  private val interps: mutable.HashMap[String, Interpreter] = mutable.HashMap[String, Interpreter]()
  private val trans: mutable.HashMap[(String, String), Translator] = mutable.HashMap[(String, String), Translator]()
  private val gens: mutable.HashMap[(String, String), Transpiler] = mutable.HashMap[(String, String), Transpiler]()
  private val binds: mutable.HashMap[String, Vector[String]] = mutable.HashMap[String, Vector[String]]()
  
  def run(): Unit = {
    setDefaults()
    println(s"$welcome\n")
    consoleLoop()
  }
  
  def setDefaults(): Unit = {
    bools.clear()
    nums.clear()
    interps.clear()
    trans.clear()
    gens.clear()
    binds.clear()
    
    bools ++= EsoDefaults.defBoolVec.map(t => (t._1, t._2))
    nums ++= EsoDefaults.defNumVec.map(t => (t._1, t._2))
    interps ++= EsoDefaults.defInterpVec.map(i => (i.name, i))
    trans ++= EsoDefaults.defTransVec.map(t => (t.id, t))
    gens ++= EsoDefaults.defGenVec.map(g => (g.id, g))
    loadBindsHandler(EsoDefaults.defBindFile, Vector()) map{bs => binds ++= bs}
  }
  
  def consoleLoop(): Unit = {
    var runChk = true
    
    while(runChk){
      val inp = StdIn.readLine(pointer).split(" ").toVector
      binds.get(inp.head) match{
        case Some(b) => execCommand(b ++ inp.tail)
        case None => execCommand(inp)
      }
    }
    
    def execCommand(inp: Vector[String]): Unit = inp match{
      case "run" +: args => runHandler(mkImmut(interps), mkImmut(trans), Config(bools, nums))(args)

      case "transpile" +: args => genHandler(Config(bools, nums), mkImmut(trans), mkImmut(gens))(args)

      case "translate" +: args => transHandler(Config(bools, nums), mkImmut(trans))(args)
      case "defineBFLang" +: _ => trans += bflMakeHandler
      case "loadBFLangs" +: args => trans ++= loadBFLHandler(args) //Not Working Properly
      case "saveBFLangs" +: args => saveBFLHandler(mkImmut(trans), args)
      case "syntax" +: args => println(syntaxHandler(mkImmut(trans))(args))

      case "bind" +: args => args match{
        case tok +: cmd => binds += ((tok, cmd))
        case _ => println("Error: Not Enough Arguments")
      }
      case "unbind" +: tok +: _ => binds -= tok
      case "clrBindings" +: _ => binds.clear()
      case "loadBindings" +: args => loadBindsHandler(EsoDefaults.defBindFile, args) foreach{bs => binds ++= bs}
      case "saveBindings" +: args => saveBindsHandler(EsoDefaults.defBindFile, mkImmut(binds), args)
      case "listBindings" +: _ => println(listBindsHandler(mkImmut(binds)))

      case "set" +: args => setVarHandler(Config(bools, nums))(args) match{
        case Some(op) => op match{
          case Left(p) => bools += p
          case Right(p) => nums += p
        }
        case None => println("Error: Parameter Name/Value Not Recognized")
      }
      case "defaults" +: _ => setDefaults()

      case "listLangs" +: _ => println(listLangsHandler(mkImmut(interps), mkImmut(trans), mkImmut(gens)))
      case "listVars" +: _ => println(listVarsHandler(Config(bools, nums), EsoDefaults.defDesc))
      case "help" +: _ => println(helpText)

      case "exit" +: _ =>
        println("Closing...")
        runChk = false

      case _ => println("Error: Invalid Command")
    }
  }
}
