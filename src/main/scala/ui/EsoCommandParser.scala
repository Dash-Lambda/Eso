package ui

import common.EsoObj
import parsers.{EsoParser, RegexParser}

import scala.collection.immutable

case class EsoCmd(cmd: String, args: immutable.HashMap[String, String])

object EsoCommandParser extends EsoObj{
  private val cmdReg = raw"""^(\S+)(.*)\z""".r
  private val argParser: EsoParser[String, (String, String)] = {
    RegexParser(raw"""[^-]*-(\S*) (\S*)""".r){
      m => (m.group(1), m.group(2))}}
  
  def apply(str: String): Option[EsoCmd] = str match{
    case cmdReg(c, as) => Some(EsoCmd(c, mkMap(argParser.parseAllValues(as))))
    case _ => None}
}
