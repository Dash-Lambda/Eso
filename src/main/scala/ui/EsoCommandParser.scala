package ui

import common.{EsoObj, EsoParser, RegexParser}

import scala.collection.immutable

trait EsoParsed
object ParseFail extends EsoParsed
case class EsoCmd(cmd: String, args: immutable.HashMap[String, String]) extends EsoParsed
object EsoCommandParser extends EsoObj{
  private val cmdReg = raw"""^(\S*)(.*)\z""".r
  private val argParser: EsoParser[String, (String, String)] = RegexParser[(String, String)](raw"""[^-]*-(\S*) (\S*)(.*)\z""".r){m => (m.group(1), m.group(2))}
  
  def apply(str: String): EsoParsed = str match{
    case cmdReg(c, as) => EsoCmd(c, mkMap(argParser.parseAll(as)))
    case _ => ParseFail}
}
