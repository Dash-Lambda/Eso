package ui

import common.EsoObj

import scala.annotation.tailrec
import scala.collection.immutable
import scala.io.StdIn
import scala.util.matching.Regex

object Persistent extends EsoObj{
  val exec: EsoExecutor = EsoExecutor(EsoDefaults.persistentHandlers)
  val bindReg: Regex = raw"""bind (\w+) (.*)\z""".r
  val unbindReg: Regex = raw"""unbind (.*)\z""".r
  
  def start(): Unit = {
    println(EsoDefaults.defWelcome)
    run(LoadBindingsHandler(EsoRunState.default)(immutable.HashMap()))}
  
  @tailrec
  def run(state: EsoState = EsoRunState.default): Unit = state match{
    case EsoHalt => ()
    case s: EsoRunState => StdIn.readLine(EsoDefaults.defPointer) match{
      case bindReg(t, b) => run(s.addBind(t, b))
      case unbindReg(t) => run(s.dropBind(t))
      case cmd => run(exec(s)(cmd))}}
}
