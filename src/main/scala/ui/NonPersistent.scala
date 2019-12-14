package ui

import common.EsoObj

object NonPersistent extends EsoObj{
  val exec: EsoExecutor = EsoExecutor(EsoDefaults.nonPersistentHandlers)
  def main(args: Array[String]): Unit = args.mkString(" ") match{
    case "" | "persistent" => Persistent.start()
    case str => exec(EsoRunState.withOps(str))(str)}
}
