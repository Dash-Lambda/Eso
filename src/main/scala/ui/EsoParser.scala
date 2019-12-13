package ui

import common.EsoObj

import scala.annotation.tailrec
import scala.collection.immutable

trait EsoParsed
object ParseFail extends EsoParsed
case class EsoCmd(cmd: String, args: immutable.HashMap[String, String]) extends EsoParsed
object EsoParser extends EsoObj{
  private val cmdReg = raw"""^(\S*)(.*)\z""".r
  private val argReg = raw"""[^-]*-(\S*) (\S*)(.*)\z""".r
  
  def apply(str: String): EsoParsed = str match{
    case cmdReg(c, as) => EsoCmd(c, pArgs(as))
    case _ => ParseFail}
  
  @tailrec
  def pArgs(str: String, ac: Vector[(String, String)] = Vector()): immutable.HashMap[String, String] = str match{
    case argReg(k, v, as) => pArgs(as, ac :+ (k, v))
    case _ => mkMap(ac)}
}
