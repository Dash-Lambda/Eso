package common

case class TranslatorException(info: String) extends Throwable
case class InterpreterException(info: String) extends Throwable
