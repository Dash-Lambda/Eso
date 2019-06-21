package ui

import ConsoleHandlers._
import interpreters.BFFunctional._
import translators.{BFTranslator, BrainPuff, Ook}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.util.{Failure, Success}

object EsoTerpConsole {
  val nativeTrans: Vector[BFTranslator] = Vector[BFTranslator](BrainPuff, Ook)
  val log: Boolean = true //IMPLEMENT USER CONTROL
  val pointer: String = "ExoTerp> "
  val defaultLangFile: String = "CustomLangs.txt"
  val helpText: String =
    """|Commands...
       |  run <language> <file name>
       |  translate <from/to> <language> <source> <optional destination>
       |  loadBFLangs <optional file name>
       |  listLangs
       |  syntax <language>
       |  exit
       |  """.stripMargin
  
  def main(args: Array[String]): Unit = {
    val builder = immutable.HashMap.newBuilder[String, BFTranslator]
    builder ++= nativeTrans.map(trans => (trans.name, trans))
    val transMap = builder.result
    print("Welcome to ExoTerp, the functional esoteric language interpreter!")
    consoleLoop(transMap)
  }
  
  @tailrec
  def consoleLoop(translators: immutable.HashMap[String, BFTranslator]): Unit = {
    val inp = grabStr(s"\n$pointer").split(" ").toVector
    
    inp match{
      case "run" +: tail =>
        val res = tail match{
          case "BrainFuck" +: fnam +: _ => runProgHandler(bfRun, log)(fnam)
          case lang +: fnam +: _ => translators.get(lang) match{
            case Some(t) => runProgHandler(prog => bfRun(t(prog)), log)(fnam)
            case None => Failure(HandlerException("Language Not Recognized"))
          }
        }
        res match{
          case Success(str) => if(!log) println(str)
          case Failure(e) => println(s"Error: $e")
        }
        consoleLoop(translators)
      case "loadBFLangs" +: tail =>
        val fnam = if(tail.nonEmpty) tail.mkString(" ") else defaultLangFile
        grabBFLangs(fnam) match{
          case Success(vec) => println(s"Successfully loaded...\n${vec.map(_.name).mkString("\n")}")
            consoleLoop(translators ++ vec.map(trans => (trans.name, trans)))
          case Failure(e) => println(s"Failed to load: $e")
            consoleLoop(translators)
        }
      case "translate" +: dr +: lang +: tail =>
        translators.get(lang) match{
          case Some(trans) =>
            translateHandler(trans, dr)(tail) match{
              case Success(str) => println(s"Program successfully translated...\n$str")
              case Failure(e) => println(s"Error: $e")
            }
          case None => println(s"$lang is not a recognized translator.")
        }
        consoleLoop(translators)
      case "listLangs" +: _ => println(s"Currently loaded languages...\n- BrainFuck\n${translators.keys.map(nam => s"- $nam").mkString("\n")}")
        consoleLoop(translators)
      case "syntax" +: lang +: _ =>
        translators.get(lang) match{
          case Some(t) =>
            val str = t.syntax.toVector.map(p => s"  ${p._1} => ${p._2}").mkString("\n")
            println(s"Syntax for $lang:\n$str")
          case None => println(s"$lang is not a recognized translator.")
        }
        consoleLoop(translators)
      case "help" +: _ => println(helpText)
        consoleLoop(translators)
      case "exit" +: _ => println("Closing...")
      case _ => println("Unknown Command")
        consoleLoop(translators)
    }
  }
}
