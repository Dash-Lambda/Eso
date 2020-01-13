package ui

import common.EsoObj

import scala.util.matching.Regex

object NonPersistent extends EsoObj{
  val exec: EsoExecutor = EsoExecutor(EsoDefaults.nonPersistentHandlers)
  val persReg: Regex = raw"""persistent (.*)\z""".r
  def main(args: Array[String]): Unit = args.mkString(" ") match{
    case persReg(ops) => Persistent.start(EsoRunState.withOps(ops))
    case "" => Persistent.start()
    case str => exec(EsoRunState.withOps(str))(str)}
}
