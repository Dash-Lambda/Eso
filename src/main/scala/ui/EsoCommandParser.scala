package ui

import common.EsoObj
import parsers.{OrderedParser, OrderedRegexParser}

import scala.collection.immutable

trait EsoParsed{
  def isFail: Boolean}
object ParseFail extends EsoParsed{
  def isFail: Boolean = true}
case class EsoCmd(cmd: String, args: immutable.HashMap[String, String]) extends EsoParsed{
  def isFail: Boolean = false}

object EsoCommandParser extends EsoObj{
  private val cmdReg = raw"""^(\S+)(.*)\z""".r
  private val argParser: OrderedParser[String, (String, String)] = {
    OrderedRegexParser(raw"""[^-]*-(\S*) (\S*)""".r){
      m => (m.group(1), m.group(2))}}
  
  def apply(str: String): EsoParsed = str match{
    case cmdReg(c, as) => EsoCmd(c, mkMap(argParser.parseAllValues(as)))
    case _ => ParseFail}
}
