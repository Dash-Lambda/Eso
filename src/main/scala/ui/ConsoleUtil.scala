package ui

import java.io.{File, PrintWriter}

import brainfuck.{BFTranslator, GenBFT}
import common.{Config, EsoExcep, EsoObj, Generator, Interpreter, Translator}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.io.StdIn.readLine
import scala.io.Source
import scala.util.{Failure, Success, Try}

object ConsoleUtil extends EsoObj{
  val helpText: String =
    s"""|- run <language> <source file> <optional destination file>
        |
        |- generate <source language> <destination language> <source file> <destination file>
        |- translate <source language> <destination language> <source file> <destination file>
        |
        |- defineBFLang
        |- loadBFLangs <file>
        |- saveBFLangs <file>
        |- syntax <BF language>
        |
        |- bind <token> <binding>
        |- unbind <token>
        |- clrBindings
        |- loadBindings <binding file>
        |- saveBindings <binding file>
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
  
  def inputs: LazyList[Char] = LazyList.continually(readLine :+ '\n').flatten
  
  def runHandler(interps: immutable.HashMap[String, Interpreter],
                 trans: immutable.HashMap[(String, String), Translator],
                 config: Config)(args: Vector[String]): Unit = {
    def olim(res: LazyList[Char]): LazyList[Char] = config.num("olen") match{
      case -1 => res
      case n => res.take(n)
    }
    
    args match{
      case lang +: fnam +: tail =>
        val interp = interps.keys
          .map(k => (k, buildTrans(config, trans)(lang, k)))
          .collectFirst{case (k, Some(t)) => (str: String) => t(str) flatMap interps(k)(config)}
        doOrOp(interp, "Language Not Recognized"){intp =>
          doOrErr(readFile(fnam)){progRaw =>
            print(s"Building interpreter... ")
            val (i, bdr) = timeIt(intp(progRaw))
            println(s"Done in ${bdr}ms.")
            doOrErr(i map (_(inputs))){res =>
              val (flg, rdr) = timeIt{
                tryAll{
                  tail match{
                    case onam +: _ =>
                      val oFile = new PrintWriter(new File(onam))
                      for(c <- olim(res)){
                        print(c)
                        oFile.print(c)
                        oFile.flush()
                      }
                      oFile.close()
                    case _ => olim(res) foreach print
                  }}}
              flg match{
                case Success(_) => println(s"\nProgram completed in ${rdr}ms")
                case Failure(e) => println(s"\nProgram failed in ${rdr}ms\nError: $e")
              }}}}
      case _ => println("Error: Not Enough Arguments")
    }
  }
  
  def buildTrans(config: Config, trans: immutable.HashMap[(String, String), Translator])(lang1: String, lang2: String): Option[String => Try[String]] = {
    lazy val t2 = mkMap(trans.toVector.flatMap{case ((l1, l2), t) => Seq(((l1, l2), t.apply(config)(_)), ((l2, l1), t.unapply(config)(_)))})
    lazy val seed: String => Try[String] = str => Success(str)
    lazy val pairs = t2.keys.toVector
    def builder(chain: Vector[String]): String => Try[String] = chain.sliding(2)
      .map{case l1 +: l2 +: _ => t2((l1, l2)).apply(_)}
      .foldLeft(seed){case (ac, t) => (str: String) => ac(str) flatMap t}
    
    if(lang1 == lang2) Some(seed)
    else Iterator
      .iterate(Vector(Vector(lang1))){vec =>
        vec.flatMap{c =>
          val last = c.last
          pairs
            .collect{case (`last`, base) if !c.contains(base) => base}
            .map(l => c :+ l)}}
      .takeWhile(_.nonEmpty)
      .flatten
      .collectFirst{case vec if vec.last == lang2 => builder(vec)}
  }
  
  def transHandler(config: Config, trans: immutable.HashMap[(String, String), Translator])(args: Vector[String]): Unit = args match{
    case l1 +: l2 +: in +: on +: _ => doOrOp(buildTrans(config, trans)(l1, l2), "No Applicable Translation Path"){tran =>
      doOrErr(readFile(in)){progRaw =>
        doOrErr(tran(progRaw)){prog =>
          writeFile(on, prog)
          println(s"Translation saved to $on.")
        }
      }
    }
    case _ => println("Error: Not Enough Arguments")
  }
  
  def genHandler(config: Config, gens: immutable.HashMap[(String, String), Generator])(args: Vector[String]): Unit = args match{
    case l1 +: l2 +: in +: on +: _ => doOrOp(gens.get((l1, l2)), "Generator Not Recognized"){gen =>
      doOrErr(readFile(in)){progRaw =>
        doOrErr(gen(config)(progRaw)){prog =>
          writeFile(on, prog)
          println(s"Translation saved to $on.")
        }
      }
    }
  }
  
  def bflMakeHandler: ((String, String), BFTranslator) = {
    val keys = Vector('<', '>', '+', '-', '[', ']', ',', '.')
    val nam = readLine("Language Name: ")
    val kvs = keys.map(k => (k.toString, readLine(s"$k: ")))
    println
    ((nam, "BrainFuck"), GenBFT(nam, kvs))
  }
  
  def loadBFLHandler(args: Vector[String]): Vector[((String, String), BFTranslator)] = args match{
    case fnam +: _ => doOrErr(readFile(fnam)){str =>
      val lines = str.split("(\r\n|\r|\n)").toVector
      
      @tailrec
      def ldo(ac: Vector[BFTranslator], src: Vector[String]): Vector[((String, String), BFTranslator)] = src match{
        case nam +: tail if nam.startsWith("#") =>
          val pairs = tail.take(8).map(_.split("=>").toVector).collect{case k +: v +: _ => (k, v)}
          ldo(ac :+ GenBFT(nam.tail, pairs), tail.dropWhile(s => !s.startsWith("#")))
        case _ +: tail => ldo(ac, tail)
        case _ =>
          val res = ac.map(t => (t.id, t))
          res
      }
      
      ldo(Vector[BFTranslator](), lines)
    } match{
      case Some(vec) => vec
      case None => Vector[((String, String), BFTranslator)]()
    }
    case _ =>
      println("Error: Not Enough Arguments")
      Vector[((String, String), BFTranslator)]()
  }
  
  def saveBFLHandler(trans: immutable.HashMap[(String, String), Translator], args: Vector[String]): Unit = args match{
    case fnam +: _ =>
      val str = trans
        .values
        .collect{
          case t: BFTranslator =>
            s"""|#${t.name}
                |${t.syntax.map{case (k, v) => s"$k=>$v"}.mkString("\n")}""".stripMargin}
        .mkString("\n")
      writeFile(fnam, str)
    case _ => println("Error: Not Enough Arguments")
  }
  
  def syntaxHandler(trans: immutable.HashMap[(String, String), Translator])(args: Vector[String]): String = args match{
    case lang +: _ => trans.toVector.collectFirst{case (_, t: BFTranslator) if t.name == lang => t} match{
      case Some(t) => s"Syntax for $lang...\n${t.kvPairs.map{case (k, v) => s"$k: $v"}.mkString("\n")}\n"
      case None => "Error: Language Not Recognized"
    }
    case _ => "Error: Not Enough Arguments"
  }
  
  def loadBindsHandler(bfile: String, args: Vector[String]): Try[Vector[(String, Vector[String])]] = {
    val ifnam = args match{
      case fnam +: _ => fnam
      case _ => bfile
    }
    
    readFile(ifnam) map{str =>
      str.split("\n")
        .toVector
        .filter(_.nonEmpty)
        .map{ln =>
          val lst = ln.split(" ")
          (lst.head, lst.tail.toVector)}
    }
  }
  
  def saveBindsHandler(bfile: String, binds: immutable.HashMap[String, Vector[String]], args: Vector[String]): Unit = {
    val ofnam = args match{
      case fnam +: _ => fnam
      case _ => bfile
    }
    
    val bstr = binds.toVector
      .map{case (tok, cmd) => s"$tok ${cmd.mkString(" ")}"}
      .mkString("\n")
    writeFile(ofnam, bstr)
  }
  
  def setVarHandler(config: Config)(args: Vector[String]): Option[Either[(String, Boolean), (String, Int)]] = {
    args match{
      case nam +: dat +: _ => dat match{
        case "true"|"false" if config.bool.isDefinedAt(nam) => Some(Left((nam, dat == "true")))
        case num if config.num.isDefinedAt(nam) => Try{num.toInt} match{
          case Success(n) => Some(Right(nam, n))
          case _ => None
        }
        case _ => None
      }
    }
  }
  
  def listLangsHandler(interps: immutable.HashMap[String, Interpreter],
                       trans: immutable.HashMap[(String, String), Translator],
                       comps: immutable.HashMap[(String, String), Generator]): String = {
    f"""|Languages...
        |${interps.values.map(i => s"- $i").toVector.sorted.mkString("\n")}
        |
        |Translators...
        |${trans.values.map(t => s"- $t").toVector.sorted.mkString("\n")}
        |
        |Generators...
        |${comps.keys.map{case (snam, dnam) => s"- $snam -> $dnam"}.toVector.sorted.mkString("\n")}
        |""".stripMargin
  }
  
  def listVarsHandler(config: Config, desc: immutable.HashMap[String, String]): String = {
    val pairs = (config.bools ++ config.nums).map{case (id, d) => (id, d.toString)}
    val namLen = pairs.map(_._1.length).max
    val datLen = pairs.map(_._2.length).max
    val strs = pairs.map{case (id, d) => s"- %-${namLen}s\t= %-${datLen}s\t(${desc(id)})".format(id, d)}.mkString("\n")
    s"""|Runtime Parameters...
        |$strs
        |""".stripMargin
  }
  
  def listBindsHandler(binds: immutable.HashMap[String, Vector[String]]): String = {
    val bstr = binds.toVector
      .map{case (tok, bnd) => s"- $tok: ${bnd.mkString(" ")}"}
      .sorted
      .mkString("\n")
    
    s"""|Bindings...
        |$bstr""".stripMargin
  }
  
  def readFile(fnam: String): Try[String] = Try{
    val iFile = Source.fromFile(fnam)
    val res = iFile.mkString
    iFile.close()
    res
  }
  def writeFile(fnam: String, str: String): Unit = {
    val oFile = new PrintWriter(new File(fnam))
    oFile.print(str)
    oFile.close()
  }
  
  def doOrErr[A, B](inp: Try[A])(act: A => B): Option[B] = inp match{
    case Success(i) => Some(act(i))
    case Failure(e) =>
      e match{
        case EsoExcep(info) => println(s"Error: common.EsoExcep ($info)")
        case _ => println(s"Error: $e")
      }
      None
  }
  def doOrOp[A, B](inp: Option[A], err: String)(act: A => B): Option[B] = inp match{
    case Some(_) => inp map act
    case None =>
      println(s"Error: $err")
      None
  }
  
  def timeIt[T](thing: => T): (T, Long) = {
    val t = System.currentTimeMillis
    val res = thing
    (res, System.currentTimeMillis - t)
  }
}
