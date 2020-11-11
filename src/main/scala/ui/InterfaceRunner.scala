package ui

import common.EsoObj

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.matching.Regex

abstract class InterfaceRunner extends EsoObj{
  val loaders: Vector[LoadHandler]
  def loadState(state: EsoRunState = EsoRunState.default): EsoRunState =
    loaders.foldLeft(state){
      case (s, l) => l.loadOnly(s)}
}

object NonPersistent extends InterfaceRunner{
  val loaders: Vector[LoadHandler] = EsoDefaults.nonPersistentStartupLoaders
  val exec: EsoExecutor = EsoExecutor(EsoDefaults.nonPersistentHandlers)
  val persReg: Regex = raw"""persistent (.*)\z""".r
  
  def main(args: Array[String]): Unit = args.mkString(" ") match{
    case persReg(ops) => Persistent.start(EsoRunState.withOps(ops))
    case "" => Persistent.start()
    case str => exec(loadState(EsoRunState.withOps(str)))(str)}
}

object Persistent extends InterfaceRunner{
  val loaders: Vector[LoadHandler] = EsoDefaults.persistentStartupLoaders
  val exec: EsoExecutor = EsoExecutor(EsoDefaults.persistentHandlers)
  val bindReg: Regex = raw"""bind (\w+) (.*)\z""".r
  val unbindReg: Regex = raw"""unbind (.*)\z""".r
  
  def start(state: EsoRunState = EsoRunState.default): Unit = {
    val loaded = loadState(state)
    println(EsoDefaults.defWelcome)
    run(loaded)}
  
  @tailrec
  def run(state: EsoState = EsoRunState.default): Unit = state match{
    case EsoHalt => ()
    case s: EsoRunState => StdIn.readLine(EsoDefaults.defPointer) match{
      case bindReg(t, b) => run(s.addBind(t, b))
      case unbindReg(t) => run(s.dropBind(t))
      case cmd => run(exec(s)(cmd))}}
}