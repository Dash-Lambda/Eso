package ui

import common.EsoObj

object NonPersistent extends EsoObj{
  def main(args: Array[String]): Unit = args.mkString(" ") match{
    case "" => Persistent.start()
    case str =>
      val prs = EsoParser(str)
      EsoExecutor(prs)(EsoRunState.default)}
}
