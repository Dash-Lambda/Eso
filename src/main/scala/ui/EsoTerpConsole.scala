package ui

import ConsoleHandlers._
import translators.{BFTranslator, BrainPuff, Ook}

import scala.collection.mutable

object EsoTerpConsole {
  val nativeTrans: Vector[BFTranslator] = Vector[BFTranslator](BrainPuff, Ook)
  val translators: mutable.HashMap[String, BFTranslator] = mutable.HashMap[String, BFTranslator]()
  val pointer: String = "EsoTerp> "
  val welcomeText: String =
    """|Welcome to EsoTerp, the functional esoteric language interpreter!
       |Type "help" for a list of commands.
       |""".stripMargin
  val helpText: String =
    """|Commands...
       |  run <language> <source file name>
       |  translate <source language> <target language> <source> <optional destination>
       |  loadBFLangs <file name>
       |  saveBFLangs <file name>
       |  defineBFLang
       |  listLangs
       |  syntax <language>
       |  set <variable name> <new value>
       |  listVars
       |  help
       |  exit
       |  """.stripMargin
  
  var initTapeSize: Int = 100
  var outputMaxLength: Int = -1
  var BFOpt: Boolean = true
  var log: Boolean = true
  
  def main(args: Array[String]): Unit = {
    translators ++= nativeTrans.map(t => (t.name, t))
    println(welcomeText)
    consoleLoop()
  }
  
  def printVars(): Unit = {
    val maxLen = Vector(initTapeSize, outputMaxLength, BFOpt, log).map(_.toString.length).max
    println(
      s"""|initTapeSize    = %-${maxLen}d (initial tape length for BrainFuck interpreters)
          |outputMaxLength = %-${maxLen}d (maximum size of output string for BrainFuck interpreters, useful for non-terminating programs)
          |BFOpt           = %-${maxLen}b (optimize BrainFuck code)
          |log             = %-${maxLen}b (determines whether output is shown during or after runtime)
          |""".stripMargin.format(initTapeSize, outputMaxLength, BFOpt, log))
  }
  
  def consoleLoop(): Unit = {
    var runChk = true
    while(runChk){
      val inp = grabStr(s"$pointer").split(" ").toVector
  
      inp match{
        case "run" +: args => bfRunHandler(translators, BFOpt, initTapeSize, outputMaxLength, log)(args)
        case "translate" +: args => translationHandler(translators)(args)
        case "listLangs" +: _ => println(s"Currently loaded languages...\n${translators.keys.map(s => s"- $s").mkString("\n")}")
        case "saveBFLangs" +: args => bfLangSaveHandler(translators, nativeTrans)(args)
        case "syntax" +: args => syntaxHandler(translators)(args)
        case "help" +: _ => println(helpText)
        case "listVars" +: _ => printVars()
        case "set" +: args =>
          args match{
            case "log" +: arg +: _ => log = setBoolHandler(arg, log)
            case "BFOpt" +: arg +: _ => BFOpt = setBoolHandler(arg, BFOpt)
            case "outputMaxLength" +: arg +: _ => outputMaxLength = setIntHandler(arg, outputMaxLength)
            case "initTapeSize" +: arg +: _ => initTapeSize = setIntHandler(arg, initTapeSize)
            case str +: _ => println(s"$str is not a recognized runtime parameter.")
          }
        case "loadBFLangs" +: args => translators ++= loadBFLangsHandler(args)
        case "defineBFLang" +: _ => translators += langCreationHandler
        case "exit" +: _ => println("Closing..."); runChk = false
        case _ => println("Invalid command.")
      }
    }
  }
}
