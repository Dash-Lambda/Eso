package ui

import brainfuck.{BFManaged, BFOptimize, BFToScala, FlufflePuff, Ook}
import common.{Config, EsoObj, Interpreter, Translator, Transpiler}
import deadfish.Deadfish
import emmental.Emmental
import fractran.{FracTran, FracTranpp}
import funge.{Befunge93, Befunge98}
import pdoubleprime.PDP
import scalarun.ScalaRun
import slashes.Slashes
import thue.Thue
import ui.ConsoleUtil._
import whitespace.{WSAssembly, WhiteSpace}
import wierd.Wierd

import scala.collection.{immutable, mutable}
import scala.io.StdIn

object Console extends EsoObj{
  val pointer: String = "Eso> "
  val welcome: String =
    """|Welcome to Eso, the functional esoteric language interpreter!
       |Type "help" for a list of commands.""".stripMargin
  
  val bindFile: String = "userBindings.txt"
  val interpVec: Vector[Interpreter] = Vector[Interpreter](BFManaged, WhiteSpace, FracTran, FracTranpp, Thue, PDP, ScalaRun, Slashes, Deadfish, Emmental, Befunge93, Befunge98, Wierd)
  val transVec: Vector[Translator] = Vector[Translator](FlufflePuff, Ook, WSAssembly)
  val genVec: Vector[Transpiler] = Vector[Transpiler](BFToScala)
  val boolVec: Vector[(String, Boolean, String)] = Vector[(String, Boolean, String)](
    ("log", false, "toggle detailed console logging"),
    ("dyn", false, "resize tape as needed for BF interpreter to eliminate memory limitations"),
    ("fPtr", true, "toggle whether output for P'' programs starts at the read head going right or at the end of the tape going left"),
    ("sHead", true, "toggle whether the read head starts at the beginning of the initial tape or the right end of the tape for P''"),
    ("pNull", false, "toggle whether to print the null/empty character in the output of P'' programs"),
    ("indent", false, "toggle whether or not to neatly indent generated Scala code"),
    ("dfChar", true, "toggle whether or not to print Deadfish output as char values"),
    ("bfDiv", true, "toggle whether or not divison by 0 evaluates to 0 in Befunge-98 (not yet implemented)"),
    ("bfRetCode", false, "toggle whether or not the Befunge-98 return code is displayed"))
  val numVec: Vector[(String, Int, String)] = Vector[(String, Int, String)](
    ("bfOpt", 2, "BrainFuck interpreter selection: 0=base, 1=optimized, 2=compiled"),
    ("init", 40000, "initial tape size for BrainFuck interpreter"),
    ("olen", -1, "maximum output length, useful for non-terminating programs, -1=infinite"),
    ("methSize", 1000, "maximum number of blocks in a generated method"))
  val desc: immutable.HashMap[String, String] = mkMap((boolVec ++ numVec).map{case (id, _, dc) => (id, dc)})
  
  private val bools: mutable.HashMap[String, Boolean] = mutable.HashMap[String, Boolean]()
  private val nums: mutable.HashMap[String, Int] = mutable.HashMap[String, Int]()
  private val interps: mutable.HashMap[String, Interpreter] = mutable.HashMap[String, Interpreter]()
  private val trans: mutable.HashMap[(String, String), Translator] = mutable.HashMap[(String, String), Translator]()
  private val gens: mutable.HashMap[(String, String), Transpiler] = mutable.HashMap[(String, String), Transpiler]()
  private val binds: mutable.HashMap[String, Vector[String]] = mutable.HashMap[String, Vector[String]]()
  
  def main(args: Array[String]): Unit = {
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
    
    bools ++= boolVec.map(t => (t._1, t._2))
    nums ++= numVec.map(t => (t._1, t._2))
    interps ++= interpVec.map(i => (i.name, i))
    trans ++= transVec.map(t => (t.id, t))
    gens ++= genVec.map(g => (g.id, g))
    loadBindsHandler(bindFile, Vector()) map{bs => binds ++= bs}
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
      case "urun" +: args => unsafeRun(mkImmut(interps), mkImmut(trans), Config(bools, nums))(args)
      case "run" +: args => runHandler(mkImmut(interps), mkImmut(trans), Config(bools, nums))(args)

      case "Optimize" +: fnam +: _ => readFile(fnam) flatMap BFOptimize.compOpt foreach (lst => println(lst.map(_._1).mkString))

      case "generate" +: args => genHandler(Config(bools, nums), mkImmut(gens))(args)

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
      case "loadBindings" +: args => loadBindsHandler(bindFile, args) foreach{bs => binds ++= bs}
      case "saveBindings" +: args => saveBindsHandler(bindFile, mkImmut(binds), args)
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
      case "listVars" +: _ => println(listVarsHandler(Config(bools, nums), desc))
      case "help" +: _ => println(helpText)

      case "exit" +: _ =>
        println("Closing...")
        runChk = false

      case _ => println("Error: Invalid Command")
    }
  }
}
