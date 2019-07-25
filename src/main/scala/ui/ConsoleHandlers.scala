package ui

import java.io.{File, FileWriter, PrintWriter}

import brainfuck.{BFOptimizer, BFTranslator, GenericBFTranslator}
import common.{Generator, Interpreter, InterpreterException, Translator}

import scala.annotation.tailrec
import scala.io.{Source, StdIn}
import scala.util.{Failure, Success, Try}
import scala.collection.{immutable, mutable}

case class HandlerException(info: String) extends Throwable

object ConsoleHandlers {
  val helpText: String =
    """|Commands...
       |- run <language> <source file>
       |
       |- generate <source language> <destination language> <source file> <destination file>
       |- translate <source language> <target language> <source file> <optional destination file>
       |- optimize <source file> <destination file>
       |
       |- defineBFLang
       |- loadBFLangs <file>
       |- saveBFLangs <file>
       |- syntax <BF language>
       |
       |- bind <token> <binding>
       |- unbind <token>
       |- clrBindings
       |- loadBindings <optional binding file>
       |- saveBindings <optional destination file>
       |- listBindings
       |
       |- set <variable name> <new value>
       |- defaults
       |
       |- listLangs
       |- listVars
       |- help
       |
       |- exit
       |""".stripMargin
  
  def runHandler(interpreters: mutable.HashMap[String, Interpreter],
                 bools: mutable.HashMap[String, (Boolean, String)],
                 nums: mutable.HashMap[String, (Int, String)])(args: Vector[String]): Unit = {
    (bools.get("log"), args) match{
      case (Some((log, _)), lang +: fnam +: _) if interpreters.isDefinedAt(lang) =>
        grabProg(fnam) match{
          case Success(prog) =>
            val interp: String => Try[String] = interpreters(lang)(bools, nums)
            
            print(s"Running $fnam... ")
            if(log) println
            val tStart = System.currentTimeMillis
            val res = interp(prog)
            val tDur = System.currentTimeMillis - tStart
            res match{
              case Success(_) if log => println(s"\nProgram completed in ${tDur}ms.")
              case Success(str) => print(s"Done in ${tDur}ms.\n$str\n")
              case Failure(InterpreterException(info)) => println(s"Error: $info")
              case Failure(e) => println(s"Error: $e")
            }
          case Failure(e) => println(s"Error: $e")
        }
      case _ => println("Invalid Arguments")
    }
  }
  
  def generateHandler(generators: mutable.HashMap[(String, String), Generator], bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(args: Vector[String]): Unit = args match{
    case slang +: dlang +: inam +: onam +: _ => generators.get((slang, dlang)) match{
      case Some(comp) =>
        print(s"Retrieving from $inam... ")
        Try{ Source.fromFile(inam).mkString } match{
          case Success(progRaw) =>
            println("Done.")
            comp(bools, nums)(progRaw) match{
              case Success(prog) =>
                print(s"Saving to $onam... ")
                writeFile(onam)(prog)
                println("Done.")
              case Failure(e) => println(s"Error: $e")
            }
          case Failure(e) => println(s"Error: $e")
        }
      case None => println("Compiler Not Recognized")
    }
    
    case _ => println("Not enough arguments.")
  }
  
  def optimizeHandler(bools: mutable.HashMap[String, (Boolean, String)],
                      nums: mutable.HashMap[String, (Int, String)])(args: Vector[String]): Unit = {
    args match{
      case src +: dest +: _ => grabProg(src) match{
        case Success(progRaw) => BFOptimizer(progRaw, bools("debug")._1) match{
          case Some((bops, prog)) =>
            writeFile(dest)(s"Bops:\n${bops.zipWithIndex.map{case (b, i) => s"$i: $b"}.mkString("\n")}\n\nProgram:\n${prog.zipWithIndex.map{case (p, i) => s"$i: $p"}.mkString("\n")}")
            println(s"Optimized program saved to $dest")
          case None => println("Optimizer Failure")
        }
        case Failure(e) => println(s"Error: $e")
      }
      case _ => println("Not Enough Arguments")
    }
  }
  
  def mkTrans(bools: mutable.HashMap[String, (Boolean, String)],
              nums: mutable.HashMap[String, (Int, String)],
              translators: mutable.HashMap[(String, String), Translator])
             (ilang: String)(olangs: String*): Option[String => Try[String]] = {
    def buildChain(l1: String, l2: String): Option[String => Try[String]] = {
      val links = translators.keys.flatMap{case (a, b) => Seq((a, b), (b, a))}.toVector
      
      def transGet(tl1: String, tl2: String): String => Try[String] = translators.get((tl1, tl2)) match{
        case Some(t) => t(bools, nums)
        case None => translators((tl2, tl1)).unapply(bools, nums)
      }
    
      def cLink(link: String => Try[String]): Try[String] => Try[String] = {
        case Success(str) => link(str)
        case f: Failure[String] => f
      }
    
      def cdo(src: Vector[String]): String => Try[String] = src
        .sliding(2)
        .map{case a +: b +: _ => (a, b)}
        .foldLeft((inp: String) => Success(inp): Try[String]){
          case (ac, p) => (inp: String) => cLink(transGet(p._1, p._2))(ac(inp))}
    
      Iterator
        .iterate(Vector(Vector(l1))){_
          .flatMap(vec => links.filter(_._1 == vec.last).map(p => vec :+ p._2))
          .filter(vec => vec.sizeIs == vec.distinct.length)}
        .takeWhile(_.nonEmpty)
        .map(_.collectFirst{case vec if vec.last == l2 => vec})
        .collectFirst{case Some(vec) => cdo(vec)}
    }
    def trans(l1: String, l2: String): Option[String => Try[String]] = (translators.get((l1, l2)), translators.get((l2, l1))) match{
      case (Some(t), _) => Some(t(bools, nums))
      case (_, Some(t)) => Some(t.unapply(bools, nums))
      case (_, _) => buildChain(l1, l2)
    }
    
    olangs.iterator.map(trans(ilang, _)).collectFirst{case Some(func) => func}
  }
  
  def translationHandler(bools: mutable.HashMap[String, (Boolean, String)],
                         nums: mutable.HashMap[String, (Int, String)],
                         translators: mutable.HashMap[(String, String), Translator])(args: Vector[String]): Unit = {
    val debug = bools.get("debug") match{
      case Some(p) => p._1
      case None => false
    }
    
    args match{
      case lang1 +: lang2 +: src +: tail => mkTrans(bools, nums, translators)(lang1)(lang2) match{
        case Some(t) =>
          if(debug) print(s"Retrieving program from $src... ")
          grabProg(src) match{
            case Success(prog) =>
              if(debug) println("Done.\nTranslating... ")
              t(prog) match{
                case Success(prog2) => tail match{
                  case dest +: _ =>
                    if(debug) print(s"\nSaving to $dest... ")
                    writeFile(dest)(prog2)
                    if(debug) println("Done.")
                    else println(s"Program saved to $dest")
                  case _ => println(s"Source:\n$prog\n\nTranslated:\n$prog2\n")
                }
                case Failure(e) => println(s"Error: $e")
              }
            case Failure(e) => println(s"Error: $e")
          }
        case None => println("Error: Translator Not Found")
      }
      case _ => println("Error: Invalid Arguments")
    }
  }
  
  def loadBindingsHandler(dfile: String)(implicit args: Vector[String] = Vector[String]()): Vector[(String, Vector[String])] = Try{
    val fnam = args match{
      case nam +: _ => nam
      case _ => dfile
    }
    val iFile = Source.fromFile(fnam)
    val dats = iFile.getLines.toVector.filter(_.nonEmpty)
    iFile.close()
    dats.map{str => val vec = str.split(" ").toVector; (vec.head, vec.tail)}
  } match{
    case Success(vec) =>
      println("Successfully loaded bindings")
      vec
    case Failure(e) =>
      println(s"Load Bindings Error: $e")
      Vector[(String, Vector[String])]()
  }
  
  def saveBindingsHandler(bindings: mutable.HashMap[String, Vector[String]], dfile: String)(args: Vector[String]): Unit = {
    val fnam = args.headOption match{
      case Some(str) => str
      case None => dfile
    }
    val oFile = new PrintWriter(new File(fnam))
    oFile.print(bindings.map{case (k: String, v: Vector[String]) => s"$k ${v.mkString(" ")}"}.mkString("\n"))
    oFile.close()
  }
  
  def listBindingsHandler(bindings: mutable.HashMap[String, Vector[String]]): Unit = {
    println(
      s"""|User bindings...
          |${bindings.toVector.map{case (k, v) => s"- $k => ${v.mkString(" ")}"}.mkString("\n")}
          |""".stripMargin)
  }
  
  def loadBFLangsHandler(args: Vector[String]): Vector[BFTranslator] = {
    @tailrec
    def scrub(langs: Vector[String], ac: Vector[BFTranslator]): Vector[BFTranslator] = langs match{
      case tok +: tail if tok.startsWith("name=") =>
        val nam = tok.drop(5)
        val syn = tail.take(8)
        val kvs = syn.map(str => (str.head.toString, str.drop(2)))
        scrub(tail.dropWhile(str => !str.startsWith("name=")), ac :+ GenericBFTranslator(nam, kvs))
      case _ +: tail => scrub(tail.dropWhile(str => !str.startsWith("name=")), ac)
      case _ => ac
    }
    
    args match{
      case fnam +: _ =>
        grabLines(fnam) match{
          case Success(langs) => val transVec = scrub(langs, Vector[BFTranslator]())
            println(s"Successfully loaded:\n${transVec.map(p => s"- ${p.name}").mkString("\n")}\n")
            transVec
          case Failure(e) => println(s"Error: $e")
            Vector[BFTranslator]()
        }
      case _ => println("Invalid file name.")
        Vector[BFTranslator]()
    }
  }
  
  def langCreationHandler: BFTranslator = {
    val keys = Vector("<", ">", "+", "-", "[", "]", ",", ".")
    val nam = grabStr("Language Name: ")
    val kvs = keys.map(k => (k, grabStr(s"$k: ")))
    println
    GenericBFTranslator(nam, kvs)
  }
  
  def bfLangSaveHandler(translators: mutable.HashMap[(String, String), Translator], defaults: Vector[Translator])(args: Vector[String]): Unit = args match{
    case fnam +: _ =>
      val bftrans = translators.toVector.collect{case (_, t: BFTranslator) => t}
      val oFile = new FileWriter(new File(fnam), true)
      val natLangs = defaults.map(t => t.name)
      for(lang <- bftrans.filterNot(t => natLangs.contains(t.name))){
        oFile.write(
          s"""|name=${lang.name}
              |${lang.kvPairs.map(p => s"${p._1}=${p._2}").mkString("\n")}
              |""".stripMargin)
      }
      oFile.close()
    case _ => println("Invalid arguments.")
  }
  
  def syntaxHandler(translators: mutable.HashMap[(String, String), Translator])(args: Vector[String]): Unit = {
    val bftrans = translators.toVector.collect{case (_, t: BFTranslator) => t}
    args match{
      case lang +: _ => bftrans.find(_.name == lang) match{
        case Some(t) => println(s"Syntax for $lang...\n${t.kvPairs.map{case (k, v) => s"$k: $v"}.mkString("\n")}\n")
        case None => println("Language Not Recognized")
      }
      case _ => println("Not Enough Arguments")
    }
  }
  
  def listLangsHandler(interpreters: mutable.HashMap[String, Interpreter],
                       Translators: mutable.HashMap[(String, String), Translator],
                       compilers: mutable.HashMap[(String, String), Generator]): Unit = {
    println(
      f"""|Languages...
          |${interpreters.values.map(i => s"- $i").toVector.sorted.mkString("\n")}
          |
          |Translators...
          |${Translators.values.map(t => s"- $t").toVector.sorted.mkString("\n")}
          |
          |Generators...
          |${compilers.keys.map{case (snam, dnam) => s"- $snam -> $dnam"}.toVector.sorted.mkString("\n")}
          |""".stripMargin)
  }
  
  def printVarsHandler(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)]): String = {
    val maxLen = (bools.toVector ++ nums.toVector).map(_._2._1.toString.length).max
    val maxNam = (bools.toVector ++ nums.toVector).map(_._1.toString.length).max
    val numstr = nums.toVector.sortBy(_._1).map{case (tag, (num, dsc)) => s"- %-${maxNam}s\t= %-${maxLen}d\t($dsc)".format(tag, num)}.mkString("\n")
    val boostr = bools.toVector.sortBy(_._1).map{case (tag, (boo, dsc)) => s"- %-${maxNam}s\t= %-${maxLen}b\t($dsc)".format(tag, boo)}.mkString("\n")
    s"""|Runtime Parameters...
        |$numstr
        |$boostr
        |""".stripMargin
  }
  
  def setVarHandler(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(args: Vector[String]): String = args match{
    case nam +: arg +: _ => (bools.get(nam), nums.get(nam)) match{
      case (Some((boo, dsc)), None) if arg == "true" || arg == "false" =>
        bools += nam -> (arg == "true", dsc)
        s"$nam changed from $boo to $arg"
      case (None, Some((num, dsc))) => Try{arg.toInt} match{
        case Success(n) =>
          nums += nam -> (n, dsc)
          s"$nam changed from $num to $n"
        case Failure(_) => "Error: Invalid Argument"
      }
      case (Some(_), Some(_)) => "Error: Variable is Defined Twice"
      case _ => "Error: Variable Not Recognized"
    }
  }
  
  def writeFile(fnam: String)(str: String): Unit = {
    val oFile = new PrintWriter(new File(fnam))
    oFile.print(str)
    oFile.close()
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
  
  def mkMap[A, B](vec: Vector[(A, B)]): immutable.HashMap[A, B] = {
    val builder = immutable.HashMap.newBuilder[A, B]
    builder ++= vec
    builder.result
  }
}