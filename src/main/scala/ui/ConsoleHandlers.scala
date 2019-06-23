package ui

import java.io.{File, FileWriter, PrintWriter}

import translators._

import scala.annotation.tailrec
import scala.io.{Source, StdIn}
import scala.util.{Failure, Success, Try}
import interpreters.BFOptimized._
import interpreters.BFFunctional._
import interpreters.InterpreterException

import scala.collection.mutable

case class HandlerException(info: String) extends Throwable

object ConsoleHandlers {
  def bfRunHandler(translators: mutable.HashMap[String, BFTranslator], optimized: Boolean, initTapeSize: Int, outputMaxLength: Int, log: Boolean)(args: Vector[String]): Unit = {
    def interp(str: String): Try[String] = if(optimized) bfoRun(str, initTapeSize, outputMaxLength, log) else bfRun(str, initTapeSize, outputMaxLength, log)
    args match{
      case lang +: fnam +: _ if lang == "BrainFuck" || translators.isDefinedAt(lang) =>
        grabProg(fnam) match{
          case Success(prog) =>
            print(s"Running $fnam... ")
            if(log) println
            val tStart = System.currentTimeMillis
            val res = if(lang == "BrainFuck") interp(prog) else interp(translators(lang)(prog))
            val tDur = System.currentTimeMillis - tStart
            res match{
              case Success(_) if log => println(s"\nProgram completed in ${tDur}ms.")
              case Success(str) => print(s"Done in ${tDur}ms.\n$str\n")
              case Failure(InterpreterException(info)) => println(s"Error: $info")
              case Failure(e) => println(s"Error: $e")
            }
          case Failure(e) => println(s"Error: $e")
        }
      case _ => println("Language not recognized.")
    }
  }
  
  def translationHandler(translators: mutable.HashMap[String, BFTranslator])(args: Vector[String]): Unit = {
    def chkLang(lang: String): Boolean = (lang == "BrainFuck") || translators.isDefinedAt(lang)
    def transToBF(prog: String, srcLang: String): String = if(srcLang == "BrainFuck") prog else translators(srcLang)(prog)
    def transFromBF(prog: String, dstLang: String): String = if(dstLang == "BrainFuck") prog else translators(dstLang).unapply(prog)
    def translate(prog: String, slang: String, dlang: String): String = transFromBF(transToBF(prog, slang), dlang)
    
    args match{
      case lang1 +: lang2 +: src +: tail if chkLang(lang1) && chkLang(lang2) =>
        grabProg(src) match{
          case Success(prog) =>
            println(s"Program successfully retrieved from $src.")
            tail match{
              case dest +: _ =>
                print(s"Saving to $dest... ")
                val oFile = new PrintWriter(new File(dest))
                oFile.print(translate(prog, lang1, lang2))
                oFile.close()
                println("Done.")
              case _ =>
                println(
                  s"""|Source:
                      |$prog
                      |
                      |Translated:
                      |${translate(prog, lang1, lang2)}
                      |""".stripMargin)
            }
          case Failure(e) => println(s"Error: $e")
        }
      case _ => println("Arguments not recognized.")
    }
  }
  
  def loadBFLangsHandler(args: Vector[String]): Vector[(String, BFTranslator)] = {
    @tailrec
    def scrub(langs: Vector[String], ac: Vector[(String, BFTranslator)]): Vector[(String, BFTranslator)] = langs match{
      case tok +: tail if tok.startsWith("name=") =>
        val nam = tok.drop(5)
        val syn = tail.take(8)
        val kvs = syn.map(str => (str.head.toString, str.drop(2)))
        scrub(tail.dropWhile(str => !str.startsWith("name=")), ac :+ (nam, GenericBFTranslator(nam, kvs)))
      case _ +: tail => scrub(tail.dropWhile(str => !str.startsWith("name=")), ac)
      case _ => ac
    }
    
    args match{
      case fnam +: _ =>
        grabLines(fnam) match{
          case Success(langs) => val transVec = scrub(langs, Vector[(String, BFTranslator)]())
            println(s"Successfully loaded:\n${transVec.map(p => s"- ${p._1}").mkString("\n")}\n")
            transVec
          case Failure(e) => println(s"Error: $e")
            Vector[(String, BFTranslator)]()
        }
      case _ => println("Invalid file name.")
        Vector[(String, BFTranslator)]()
    }
  }
  
  def langCreationHandler: (String, BFTranslator) = {
    val keys = Vector("<", ">", "+", "-", "[", "]", ",", ".")
    val nam = grabStr("Language Name: ")
    val kvs = keys.map(k => (k, grabStr(s"$k: ")))
    println
    (nam, GenericBFTranslator(nam, kvs))
  }
  
  def bfLangSaveHandler(translators: mutable.HashMap[String, BFTranslator], defaults: Vector[BFTranslator])(args: Vector[String]): Unit = args match{
    case fnam +: _ =>
      val oFile = new FileWriter(new File(fnam), true)
      val natLangs = defaults.map(t => (t.name, t))
      for(lang <- translators.filterNot(natLangs.contains(_)).values){
        oFile.write(
          s"""|name=${lang.name}
              |${lang.kvPairs.map(p => s"${p._1}=${p._2}").mkString("\n")}
              |""".stripMargin)
      }
      oFile.close()
    case _ => println("Invalid arguments.")
  }
  
  def syntaxHandler(translators: mutable.HashMap[String, BFTranslator])(args: Vector[String]): Unit = args match{
    case lang +: _ if translators.contains(lang) =>
      val synStr = translators(lang).kvPairs.map{case (k, v) => s"$k: $v"}.mkString("\n")
      println(s"Syntax for $lang...\n$synStr\n")
    case _ => println("Not a recognized translator.")
  }
  
  def setBoolHandler(arg: String, default: Boolean): Boolean = arg match{
    case "true" => true
    case "false" => false
    case _ =>
      println("Error: Invalid Argument")
      default
  }
  def setIntHandler(arg: String, default: Int): Int = Try{arg.toInt} match{
    case Success(n) => n
    case Failure(_) =>
      println("Error: Invalid Argument")
      default
  }
  
  def grabStr(prompt: String): String = {print(prompt); StdIn.readLine}
  def grabLines(fnam: String): Try[Vector[String]] = Try{
    val fin = Source.fromFile(fnam)
    val strs = fin.getLines.toVector
    fin.close()
    strs
  }
  def grabProg(fnam: String): Try[String] = Try{
    val fin = Source.fromFile(fnam)
    val str = fin.mkString
    fin.close()
    str
  }
}
