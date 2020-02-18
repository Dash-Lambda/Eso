package parsers

import scala.util.{Failure, Success, Try}

case class ErrorParser[A, +B](parser: EsoParser[A, B]) extends EsoParser[A, B]{
  def apply(inp: A): EsoParseRes[A, B] = Try{parser(inp)} match{
    case Success(res) => res
    case Failure(e) => EsoParseErr(e)}
}
