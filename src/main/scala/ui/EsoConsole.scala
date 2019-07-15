package ui

import ConsoleHandlers._
import assemblers.{Assembler, WhiteSpaceAssembler}
import interpreters.{Interpreter, WhiteSpace, WhiteSpaceSL}
import translators.{BFTranslator, FlufflePuff, Ook}

import scala.collection.{immutable, mutable}

object EsoConsole {
  val pointer: String = "Eso> "
  val welcomeText: String =
    """|Welcome to Eso, the functional esoteric language interpreter!
       |Type "help" for a list of commands.
       |""".stripMargin
  val defaultBindingFile: String = "userBindings.txt"
  val nativeTrans: Vector[BFTranslator] = Vector[BFTranslator](FlufflePuff, Ook)
  val assemVec: Vector[(String, Assembler)] = Vector[(String, Assembler)](("WhiteSpace", WhiteSpaceAssembler))
  val interpVec: Vector[(String, Interpreter)] = Vector[(String, Interpreter)](("WhiteSpace", WhiteSpace), ("WhiteSpaceSL", WhiteSpaceSL))
  
  val BFTranslators: mutable.HashMap[String, BFTranslator] = mutable.HashMap[String, BFTranslator]()
  val userBindings: mutable.HashMap[String, Vector[String]] = mutable.HashMap[String, Vector[String]]()
  var initTapeSize: Int = 40000
  var outputMaxLength: Int = -1
  var BFOpt: Int = 1
  var log: Boolean = true
  var debug: Boolean = false
  var dynamicTapeSize: Boolean = false
  
  def main(args: Array[String]): Unit = {
    BFTranslators ++= nativeTrans.map(t => (t.name, t))
    userBindings ++= loadBindingsHandler(defaultBindingFile)
    val interpreters = mkMap(interpVec)
    val assemblers = mkMap(assemVec)
    println(welcomeText)
    consoleLoop(interpreters, assemblers)
  }
  
  def consoleLoop(interpreters: immutable.HashMap[String, Interpreter], assemblers: immutable.HashMap[String, Assembler]): Unit = {
    var runChk = true
    
    while(runChk){
      val inp = grabStr(s"$pointer").split(" ").toVector
      
      userBindings.get(inp.head) match{
        case Some(udb) => execCommand(udb ++ inp.tail)
        case None => execCommand(inp)
      }
    }
    
    def execCommand(inp: Vector[String]): Unit = inp match{
      case "run" +: args => runHandler(BFTranslators, interpreters, BFOpt, initTapeSize, outputMaxLength, dynamicTapeSize, log, debug)(args)

      case "compile" +: args => compileHandler(initTapeSize, outputMaxLength, dynamicTapeSize, log, debug)(args)
      
      case "assemble" +: args => assembleHandler(assemblers, log, rev = false)(args)
      case "disassemble" +: args => assembleHandler(assemblers, log, rev = true)(args)

      case "optimize" +: args => optimizeHandler(args, debug)
      
      case "translate" +: args => translationHandler(BFTranslators)(args)
      case "defineBFLang" +: _ => BFTranslators += langCreationHandler
      case "loadBFLangs" +: args => BFTranslators ++= loadBFLangsHandler(args)
      case "saveBFLangs" +: args => bfLangSaveHandler(BFTranslators, nativeTrans)(args)
      case "syntax" +: args => syntaxHandler(BFTranslators)(args)
      
      case "bind" +: tok +: args => userBindings += ((tok, args))
      case "unbind" +: tok +: _ => userBindings -= tok
      case "clrBindings" +: _ => userBindings.clear
      case "loadBindings" +: fnam +: _ => userBindings ++= loadBindingsHandler(fnam)
      case "saveBindings" +: fnam +: _ => saveBindingsHandler(userBindings)(fnam)
      case "listBindings" +: _ => listBindingsHandler(userBindings)
      
      case "set" +: args =>
        args match{
          case "log" +: arg +: _ => log = setBoolHandler(arg, log)
          case "debug" +: arg +: _ => debug = setBoolHandler(arg, debug)
          case "dynamicTapeSize" +: arg +: _ => dynamicTapeSize = setBoolHandler(arg, dynamicTapeSize)
          case "BFOpt" +: arg +: _ => BFOpt = setIntHandler(arg, BFOpt)
          case "outputMaxLength" +: arg +: _ => outputMaxLength = setIntHandler(arg, outputMaxLength)
          case "initTapeSize" +: arg +: _ => initTapeSize = setIntHandler(arg, initTapeSize)
          case str +: _ => println(s"$str is not a recognized runtime parameter.")
        }
        
      case "listLangs" +: _ => printLangsHandler(interpreters, BFTranslators, assemblers)
      case "listVars" +: _ => printVarsHandler(initTapeSize, outputMaxLength, BFOpt, dynamicTapeSize, log, debug)
      case "help" +: _ => helpHandler()
      
      case "exit" +: _ => println("Closing..."); runChk = false
      
      case _ => println("Invalid command.")
    }
  }
}