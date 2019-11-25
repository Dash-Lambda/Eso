package common

case class EsoExcep(info: String) extends Throwable
object UnimplementedException extends Throwable