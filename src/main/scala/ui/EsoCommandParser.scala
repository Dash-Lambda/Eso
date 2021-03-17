package ui

import common.EsoObj
import parsers.EsoParser
import parsers.Implicits._

import scala.collection.immutable

object EsoCommandParser extends EsoObj{
  val cmdParse: EsoParser[(String, immutable.HashMap[String, String])] = {
    val opParse: EsoParser[String] = """^(\S+)""".r
    val argParse: EsoParser[(String, String)] = ("""^[^-]*-""".r &> """^\S*""".r) <&> ("""^\s*""".r &> """^\S*""".r)
    opParse <&> (argParse.* map (mkMap(_)))}
  
  def apply(str: String): Option[(String, immutable.HashMap[String, String])] = cmdParse(str).get
}