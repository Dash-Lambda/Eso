package ui

import common.EsoObj

import scala.collection.immutable
import scala.util.matching.Regex

case class EsoExecutor(cmds: Vector[CommandHandler]) extends EsoObj{
  val boundReg: Regex = raw"""(\w+)((?: .*)?)\z""".r
  val handlers: immutable.HashMap[String, CommandHandler] = mkMap(cmds map (h => (h.nam, h)))
  
  def apply(state: EsoRunState)(inp: String): EsoState = parse(state.binds)(inp) match{
    case Some(res) => res match{
      case ("help", _) =>
        showHelp()
        state
      case (cmd, args) => handlers.get(cmd) match{
        case Some(h) => h(state)(args)
        case None =>
          println(s"Error: Invalid Command (Unknown Handler $cmd)")
          state}}
    case _ =>
      println("Error: Invalid Command (Parse Fail)")
      state}
  
  def parse(binds: immutable.HashMap[String, String])(inp: String): Option[(String, immutable.HashMap[String, String])] = inp match{
    case boundReg(b, ops) if binds.isDefinedAt(b) => EsoCommandParser(s"${binds(b)}$ops")
    case _ => EsoCommandParser(inp)}
  
  def showHelp(): Unit = {
    val len = cmds.map(_.nam.length).max
    val cStr = cmds.sortBy(_.nam).map(h => s"  %${len}s: ${h.helpStr}".format(h.nam)).mkString("\n")
    val hStr =
      s"""|Version: ${getClass.getPackage.getImplementationVersion}
          |Commands:
          |$cStr
          |
          |Syntax:
          |  <expr>: Required
          |  [expr]: At least one of
          |  {expr}: Optional
          |   expr*: Repeated any number of times
          |""".stripMargin
    println(hStr)}
}
