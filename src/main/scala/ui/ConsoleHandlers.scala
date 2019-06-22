package ui

import java.io.{File, FileWriter, PrintWriter}

import translators._

import scala.annotation.tailrec
import scala.io.{Source, StdIn}
import scala.util.{Failure, Success, Try}
import interpreters.BFOptimized._
import interpreters.BFFunctional._
import interpreters.InterpreterException

import scala.collection.immutable

case class HandlerException(info: String) extends Throwable

object ConsoleHandlers {
  def bfRunHandler(translators: immutable.HashMap[String, BFTranslator], optimized: Boolean)(args: Vector[String]): Unit = {
    def interp(str: String): Try[String] = if(optimized) bfoRun(str) else bfRun(str)
    args match{
      case lang +: fnam +: _ if lang == "BrainFuck" || translators.isDefinedAt(lang) =>
        grabProg(fnam) match{
          case Success(prog) =>
            println(s"Running $fnam...")
            val res = if(lang == "BrainFuck") interp(prog) else interp(translators(lang)(prog))
            res match{
              case Success(_) => println("\nProgram completed successfully.")
              case Failure(InterpreterException(info)) => println(s"Error: $info")
              case Failure(e) => println(s"Error: $e")
            }
          case Failure(e) => println(s"Error: $e")
        }
      case _ => println("Language not recognized.")
    }
  }
  
  def translationHandler(translators: immutable.HashMap[String, BFTranslator])(args: Vector[String]): Unit = {
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
                      |
                      |$prog
                      |
                      |Translated:
                      |
                      |${translate(prog, lang1, lang2)}""".stripMargin)
            }
          case Failure(e) => println(s"Error: $e")
        }
      case _ => println("Arguments not recognized.")
    }
  }
  
  def loadLangsHandler(args: Vector[String]): Vector[(String, BFTranslator)] = {
    @tailrec
    def scrub(langs: Vector[String], ac: Vector[(String, BFTranslator)]): Vector[(String, BFTranslator)] = langs match{
      case tok +: tail if tok.startsWith("name=") =>
        val nam = tok.drop(5)
        val syn = tail.take(8)
        val kvs = syn.map(str => (str.head.toString, str.drop(2)))
        scrub(langs.dropWhile(str => !str.startsWith("name=")), ac :+ (nam, GenericBFTranslator(nam, kvs)))
    }
    
    args match{
      case fnam +: _ =>
        grabProg(fnam) match{
          case Success(langs) => val transVec = scrub(langs.split("\n").toVector, Vector[(String, BFTranslator)]())
            println(s"Successfully loaded:\n${transVec.map(p => s"- ${p._1}").mkString("\n")}")
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
    
    (nam, GenericBFTranslator(nam, kvs))
  }
  
  def bfLangSaveHandler(translators: immutable.HashMap[String, BFTranslator], defaults: Vector[BFTranslator])(args: Vector[String]): Unit = args match{
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
  
  def syntaxHandler(translators: immutable.HashMap[String, BFTranslator])(args: Vector[String]): Unit = args match{
    case lang +: _ if translators.isDefinedAt(lang) =>
      val synStr = translators(lang).kvPairs.map{case (k, v) => s"$k: $v"}.mkString("\n")
      println(s"Syntax for $lang...\n$synStr")
    case _ => println("Not a recognized translator.")
  }
  
  def grabStr(prompt: String): String = {print(prompt); StdIn.readLine}
  def grabProg(fnam: String): Try[String] = Try{
    val fin = Source.fromFile(fnam)
    val str = fin.mkString
    fin.close()
    str
  }
}
