package ui

import ConsoleHandlers._
import translators.{BFTranslator, BrainPuff, Ook}

import scala.annotation.tailrec
import scala.collection.immutable

object EsoTerpConsole {
  val nativeTrans: Vector[BFTranslator] = Vector[BFTranslator](BrainPuff, Ook)
  val welcomeText = "Welcome to EsoTerp, the functional esoteric language interpreter!"
  val pointer: String = "EsoTerp> "
  val helpText: String =
    """|Commands...
       |  run <language> <source file name>
       |  runOp <language> <source file name>
       |  translate <from/to> <language> <source> <optional destination>
       |  loadBFLangs <file name>
       |  saveBFLangs <file name>
       |  defineBFLang
       |  listLangs
       |  syntax <language>
       |  exit
       |  """.stripMargin
  
  def main(args: Array[String]): Unit = {
    val builder = immutable.HashMap.newBuilder[String, BFTranslator]
    builder ++= nativeTrans.map(trans => (trans.name, trans))
    val transMap = builder.result
    print(welcomeText)
    consoleLoop(transMap)
  }
  
  @tailrec
  def consoleLoop(translators: immutable.HashMap[String, BFTranslator]): Unit = {
    val inp = grabStr(s"\n$pointer").split(" ").toVector
    
    inp match{
      case "run" +: args => bfRunHandler(translators, optimized = false)(args)
        consoleLoop(translators)
      case "runOp" +: args => bfRunHandler(translators, optimized = true)(args)
        consoleLoop(translators)
      case "translate" +: args => translationHandler(translators)(args)
        consoleLoop(translators)
      case "listLangs" +: _ => println(s"Currently loaded languages...\n${translators.keys.map(s => s"- $s").mkString("\n")}")
        consoleLoop(translators)
      case "saveBFLangs" +: args => bfLangSaveHandler(translators, nativeTrans)(args)
        consoleLoop(translators)
      case "syntax" +: args => syntaxHandler(translators)(args)
        consoleLoop(translators)
      case "loadBFLangs" +: args => consoleLoop(translators ++ loadLangsHandler(args))
      case "defineBFLang" +: _ => consoleLoop(translators + langCreationHandler)
      case "exit" +: _ => println("Closing...")
      case _ => println("Invalid command.")
        consoleLoop(translators)
    }
  }
}
