package thue

import common.{Interpreter, InterpreterException}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn
import scala.util.{Failure, Random, Success, Try}

object Thue extends Interpreter{
  val name: String = "Thue"
  
  def apply1(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(progRaw: String): Try[String] = {
    val str =
      s"""|a::=b
          |c::=
          |::=d
          |::=
          |e""".stripMargin
    val cond = str
      .split("(\r\n|\r|\n)")
      .takeWhile(_ != "::=")
      .map(str => (str, str.indexOf("::=")))
      .collect{case (str, i) if i > 0 => (str.take(i), str.drop(i + 3))}
    println(cond.mkString("\n"))
    Success(str)
  }
  def apply(bools: mutable.HashMap[String, (Boolean, String)], nums: mutable.HashMap[String, (Int, String)])(progRaw: String): Try[String] = {
    getParms(bools, nums)("log")() match{
      case Some((log +: _, _)) => condition(progRaw) match{
        case Some((init, prog)) =>
          if(prog.nonEmpty) Success(eval(prog, init, log))
          else Failure(InterpreterException("No Initial Value"))
        case None => Failure(InterpreterException("Malformed Program"))
      }
      case None => Failure(InterpreterException("Unspecified Configuration Parameters: log"))
    }
  }
  
  def eval(prog: Vector[(String, String)], init: String, log: Boolean): String = {
    val rand = new Random()
    @tailrec
    def tso(stat: String, res: String): String = {
      val hits = prog.filter(p => stat.contains(p._1))
      if(hits.isEmpty){
        if(res.nonEmpty) res
        else{
          if(log) println(stat)
          stat
        }}
      else{
        val (k, v) = hits(rand.nextInt(hits.length))
        v match{
          case ":::" => tso(stat.replaceAllLiterally(k, StdIn.readLine), res)
          case _ =>
            if(v.startsWith("~")) {
              if (log) print(v.tail)
              tso(stat.replaceAllLiterally(k, ""), res ++ v.tail)
            }else tso(stat.replaceAllLiterally(k, v), res)
        }
      }
    }
    
    tso(init, "")
  }
  
  def condition(progRaw: String): Option[(String, Vector[(String, String)])] = {
    val lines = progRaw
      .split("(\r\n|\r|\n)")
      .filter(_.nonEmpty)
      .map(str => (str, str.indexOf("::=")))
    lazy val prog = lines
      .takeWhile(_._2 != 0)
      .collect{case (str, i) if i > 0 => (str.take(i), str.drop(i + 3))}
      .toVector
    
    lines
      .dropWhile(_._2 != 0)
      .collectFirst{case (str, i) if i == -1 => (str, prog)}
  }
}
