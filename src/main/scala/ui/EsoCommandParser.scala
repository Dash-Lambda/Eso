package ui

import common.EsoObj
import parsers.{EsoParser, RegexParser}

import scala.collection.immutable

object EsoCommandParser extends EsoObj{
  val cmdParser: EsoParser[String, (String, immutable.HashMap[String, String])] = {
    val opParser = RegexParser(raw"""^(\S+)\s*""")(m => m.group(1))
    val argParser = RegexParser(raw"""[^-]*-(\S*) (\S*)""")(m => (m.group(1), m.group(2)))
    opParser <&> argParser.*.map(mkMap(_))}
  
  def apply(str: String): Option[(String, immutable.HashMap[String, String])] = cmdParser(str).toOption
}