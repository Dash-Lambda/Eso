package ui

import common.EsoObj
import parsers.EsoParser
import parsers.EsoParser._

import scala.collection.immutable

object EsoCommandParser extends EsoObj{
  val cmdParse: EsoParser[(String, immutable.HashMap[String, String])] = {
    val opParse: EsoParser[String] = R("""^(\S+)""".r)
    val argParse: EsoParser[(String, String)] = (R("""^[^-]*-""".r) &> R("""^\S*""".r)) <&> (R("""^\s*""".r) &> R("""^\S*""".r))
    opParse <&> (argParse.* map (mkMap(_)))}
  
  def apply(str: String): Option[(String, immutable.HashMap[String, String])] = cmdParse(str).get.map(_._1)
}