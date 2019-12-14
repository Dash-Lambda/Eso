package ui

import common.EsoObj

import scala.collection.immutable
import scala.util.matching.Regex

case class EsoExecutor(cmds: Vector[InterfaceHandler]) extends EsoObj{
  val boundReg: Regex = raw"""(\w+)((?: .*)?)\z""".r
  val handlers: immutable.HashMap[String, InterfaceHandler] = mkMap(cmds map (h => (h.nam, h)))
  
  def apply(state: EsoRunState)(inp: String): EsoState = parse(state.binds)(inp) match{
    case EsoCmd("help", args) =>
      showHelp()
      state
    case EsoCmd(cmd, args) => handlers.get(cmd) match{
      case Some(h) => h(state)(args)
      case None =>
        println("Error: Invalid Command")
        state}
    case ParseFail =>
      println("Error: Invalid Command")
      state}
  
  def parse(binds: immutable.HashMap[String, String])(inp: String): EsoParsed = inp match{
    case boundReg(b, ops) if binds.isDefinedAt(b) => EsoParser(s"${binds(b)}$ops")
    case _ => EsoParser(inp)}
  
  def showHelp(): Unit = {
    val cStr = cmds.map(h => s"- ${h.nam} ${h.helpStr}").sorted.mkString("\n")
    val hStr =
      s"""|Commands:
          |$cStr
          |
          |Syntax:
          |<expr>: Required
          |{expr}: Optional
          |expr*: Repeated any number of times
          |""".stripMargin
    println(hStr)}
}
