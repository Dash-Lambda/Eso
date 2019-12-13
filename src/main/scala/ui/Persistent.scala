package ui

import common.EsoObj
import InterfaceHandlers._

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.matching.Regex

object Persistent extends EsoObj{
  val bindReg: Regex = raw"""bind (\w+) (.*)\z""".r
  val unbindReg: Regex = raw"""unbind (.*)\z""".r
  val boundReg: Regex = raw"""(\w+)(.*)\z""".r
  
  def start(): Unit = {
    println(EsoDefaults.defWelcome)
    run()
  }
  
  @tailrec
  def run(state: EsoState = EsoRunState.default): Unit = state match{
    case EsoHalt => ()
    case s: EsoRunState => StdIn.readLine(EsoDefaults.defPointer) match{
      case bindReg(t, b) => run(s.addBind(t, b))
      case unbindReg(t) => run(s.dropBind(t))
      case boundReg(b, ops) if s.binds.isDefinedAt(b) =>
        val prs = EsoParser(s"${s.binds(b)} $ops")
        val nxt = EsoExecutor(prs)(s)
        run(nxt)
      case cmd =>
        val prs = EsoParser(cmd)
        val nxt = EsoExecutor(prs)(s)
        run(nxt)}}
}
