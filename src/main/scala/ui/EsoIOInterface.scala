package ui

import common.EsoObj

import scala.io.StdIn

trait EsoIOInterface extends EsoObj{
  def readLine: String
  def readLine(str: String): String
  def linesIterator: Iterator[String] = Iterator.continually(readLine)
  def charsIterator: Iterator[Char] = linesIterator.flatMap(ln => ln + '\n')
  
  def print(x: Any): Unit
  def println(x: Any): Unit
  def println(): Unit = this.println("")
}

object EsoConsoleInterface extends EsoIOInterface{
  def readLine: String = StdIn.readLine
  def readLine(str: String): String = StdIn.readLine(str)
  
  def print(x: Any): Unit = Console.print(x)
  def println(x: Any): Unit = Console.println(x)
}

class EsoTestInterface(var inp: Seq[String], strbldr: StringBuilder) extends EsoIOInterface{
  def readLine: String = {
    val res = inp.head
    inp = inp.tail
    res}
  def readLine(str: String): String = {
    strbldr ++= str
    readLine}
  
  def print(x: Any): Unit = {
    strbldr ++= x.toString}
  def println(x: Any): Unit = {
    strbldr ++= x.toString + '\n'}
  
  def collectOutput(): String = strbldr.mkString
  def clear(): Unit = strbldr.clear()
}
object EsoTestInterface{
  def apply(inp: Seq[String]): EsoTestInterface = new EsoTestInterface(inp, new StringBuilder)
}