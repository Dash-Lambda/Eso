package ui

import common.{Config, EsoObj}
import ConsoleUtil.{doOrErr, runHandler, genHandler, transHandler, listLangsHandler, listVarsHandler}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

object NonPersistent extends EsoObj{
  val helpStr: String =
    s"""|- run <language> <source file> {optional target file} {optional arguments}
        |- transpile <source language> <destination language> <source file> <target file> {optional arguments}
        |- translate <source language> <destination language> <source file> <target file> {optional arguments}
        |- listLangs
        |- listVars
        |- persistent
        |- help
        |
        |Optional arguments are supplied as name-value pairs, e.g. "run BrainFuck mandelbrot.b -dyn true -init 1 -opt 1"
        |""".stripMargin
  
  def main(args: Array[String]): Unit = args.toVector match{
    case cmd +: args =>
      val nargs = args.takeWhile(s => !s.startsWith("-"))
      cmd match{
        case "run" => doOrErr(parseToConfig(args)){cfg => runHandler(EsoDefaults.defInterpMap, EsoDefaults.defTransMap, cfg)(nargs)}
        case "transpile" => doOrErr(parseToConfig(args)){cfg => genHandler(cfg, EsoDefaults.defTransMap, EsoDefaults.defGenMap)(nargs)}
        case "translate" => doOrErr(parseToConfig(args)){cfg => transHandler(cfg, EsoDefaults.defTransMap)(nargs)}
        case "listLangs" => println(listLangsHandler(EsoDefaults.defInterpMap, EsoDefaults.defTransMap, EsoDefaults.defGenMap))
        case "listVars" => doOrErr(parseToConfig(args)){cfg => println(listVarsHandler(cfg, EsoDefaults.defDesc))}
        case "persistent" => Console.run()
        case "help" => println(helpStr)
        case _ => println("Error: Invalid Command\nUse 'help' for list of commands.")
      }
    case _ => Console.run()
  }
  
  def parseToConfig(ops: Vector[String]): Try[Config] = Try{parseOps(ops)} map {case (bs, ns) => buildConfig(bs, ns)}
  
  def buildConfig(bs: Vector[(String, Boolean)], ns: Vector[(String, Int)]): Config = {
    val bmap = mutable.HashMap[String, Boolean]()
    val nmap = mutable.HashMap[String, Int]()
    bmap ++= EsoDefaults.defBoolVec.map{case (s, b, _) => (s, b)}
    nmap ++= EsoDefaults.defNumVec.map{case (s, n, _) => (s, n)}
    bmap ++= bs
    nmap ++= ns
    Config(bmap, nmap)
  }
  
  def parseOps(ops: Vector[String]): (Vector[(String, Boolean)], Vector[(String, Int)]) = {
    @tailrec
    def pdo(src: Vector[String], bs: Vector[(String, Boolean)], ns: Vector[(String, Int)]): (Vector[(String, Boolean)], Vector[(String, Int)]) = src match{
      case s +: ss if s.startsWith("-") =>
        if(EsoDefaults.defNumVec.map(_._1).contains(s.tail)) ss match{
          case n +: tl => pdo(tl, bs, ns :+ ((s.tail, n.toInt)))
          case _ => pdo(ss, bs, ns)
        }
        else if(EsoDefaults.defBoolVec.map(_._1).contains(s.tail)) ss match{
          case b +: tl => pdo(tl, bs :+ ((s.tail, b == "true")), ns)
          case _ => pdo(ss, bs, ns)
        }
        else pdo(ss, bs, ns)
      case _ +: ss => pdo(ss, bs, ns)
      case _ => (bs, ns)
    }
    pdo(ops, Vector(), Vector())
  }
}
