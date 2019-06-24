package ui

import ConsoleHandlers._
import assemblers.{Assembler, WhiteSpaceAssembler}
import interpreters.{Interpreter, WhiteSpace}
import translators.{BFTranslator, BrainPuff, Ook}

import scala.collection.{immutable, mutable}

object EsoTerpConsole {
  val nativeTrans: Vector[BFTranslator] = Vector[BFTranslator](BrainPuff, Ook)
  val BFTranslators: mutable.HashMap[String, BFTranslator] = mutable.HashMap[String, BFTranslator]()
  val assemVec: Vector[(String, Assembler)] = Vector[(String, Assembler)](("WhiteSpace", WhiteSpaceAssembler))
  val interpVec: Vector[(String, Interpreter)] = Vector[(String, Interpreter)](("WhiteSpace", WhiteSpace))
  val pointer: String = "EsoTerp> "
  val welcomeText: String =
    """|Welcome to EsoTerp, the functional esoteric language interpreter!
       |Type "help" for a list of commands.
       |""".stripMargin
  val helpText: String =
    """|Commands...
       |  run <language> <source file>
       |  assemble <language> <source file> <destination file>
       |  translate <source language> <target language> <source file> <optional destination file>
       |  loadBFLangs <file>
       |  saveBFLangs <file>
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
    BFTranslators ++= nativeTrans.map(t => (t.name, t))
    val interpreters = {
      val builder = immutable.HashMap.newBuilder[String, Interpreter]
      builder ++= interpVec
      builder.result
    }
    val assemblers = {
      val builder = immutable.HashMap.newBuilder[String, Assembler]
      builder ++= assemVec
      builder.result
    }
    println(welcomeText)
    consoleLoop(interpreters, assemblers)
  }
  
  def consoleLoop(interpreters: immutable.HashMap[String, Interpreter], assemblers: immutable.HashMap[String, Assembler]): Unit = {
    var runChk = true
    while(runChk){
      val inp = grabStr(s"$pointer").split(" ").toVector
  
      inp match{
        case "run" +: args => runHandler(BFTranslators, interpreters, BFOpt, initTapeSize, outputMaxLength, log)(args)
        case "assemble" +: args => assembleHandler(assemblers, log, rev = false)(args)
        case "disassemble" +: args => assembleHandler(assemblers, log, rev = true)(args)
        case "translate" +: args => translationHandler(BFTranslators)(args)
        case "listLangs" +: _ => printLangsHandler(interpreters, BFTranslators, assemblers)
        case "saveBFLangs" +: args => bfLangSaveHandler(BFTranslators, nativeTrans)(args)
        case "syntax" +: args => syntaxHandler(BFTranslators)(args)
        case "help" +: _ => println(helpText)
        case "listVars" +: _ => printVarsHandler(initTapeSize, outputMaxLength, BFOpt, log)
        case "set" +: args =>
          args match{
            case "log" +: arg +: _ => log = setBoolHandler(arg, log)
            case "BFOpt" +: arg +: _ => BFOpt = setBoolHandler(arg, BFOpt)
            case "outputMaxLength" +: arg +: _ => outputMaxLength = setIntHandler(arg, outputMaxLength)
            case "initTapeSize" +: arg +: _ => initTapeSize = setIntHandler(arg, initTapeSize)
            case str +: _ => println(s"$str is not a recognized runtime parameter.")
          }
        case "loadBFLangs" +: args => BFTranslators ++= loadBFLangsHandler(args)
        case "defineBFLang" +: _ => BFTranslators += langCreationHandler
        case "exit" +: _ => println("Closing..."); runChk = false
        case _ => println("Invalid command.")
      }
    }
  }
}
