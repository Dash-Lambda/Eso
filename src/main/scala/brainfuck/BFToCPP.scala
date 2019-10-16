package brainfuck

import scala.annotation.tailrec

object BFToCPP extends BFTranspiler{
  val dst: String = "C++"
  
  def genProg(init: Int, olen: Int, dyn: Boolean, methSize: Int, prog: LazyList[(Char, Either[Int, BlkOp])]): String = {
    def incStr(n: Int): String = s"${if(n < 0) "-" else "+"}= ${n.abs}"
    def shiftStr(n: Int): String = s"${if(n < 0) "-" else "+"} ${n.abs}"
    
    def opStr(bop: BlkOp): String = {
      val opstr = bop.ops map{
        case (0, Some(n)) => s"tape[p] ${incStr(n)};"
        case (i, Some(n)) => s"tape[p ${shiftStr(i)}] ${incStr(n)};"
        case (i, None) => s"tape[p ${shiftStr(i)}] = 0;"
      }
      s"${opstr.mkString("\n")}${if(bop.shift != 0) s"\np ${incStr(bop.shift)};" else ""}"
    }
    def lopStr(bop: BlkOp): String = {
      val opstr = bop.ops
        .filter(_._1 != 0)
        .map{
          case (i, Some(n)) => s"tape[p ${shiftStr(i)}] ${incStr(n)}*tmp;"
          case (i, None) => s"tape[p ${shiftStr(i)}] = 0;"
        }
      s"""|if(tape[p] != 0){
          |int tmp = tape[p];
          |${opstr.mkString("\n")}
          |tape[p] = 0;
          |}""".stripMargin
    }
    
    @tailrec
    def cgo(ac: Vector[String], src: LazyList[(Char, Either[Int, BlkOp])]): Vector[String] = {
      src match {
        case (op, arg) +: ops =>
          val str = arg match {
            case Left(num) => op match {
              case 'm' => s"p ${incStr(num)};"
              case '/' => s"while(tape[p] != 0){p ${incStr(num)};}"
              case '[' => "while(tape[p] != 0){"
              case ']' => "}"
              case ',' => "tape[p] = getchar();"
              case '.' => "putchar(tape[p]);"
              case 'e' => ""
            }
            case Right(bop) => op match {
              case 'u' | 'a' => opStr(bop)
              case 'l' => lopStr(bop)
            }
          }
          cgo(ac :+ str, ops)
        case _ => ac
      }
    }
    
    val methStr = cgo(Vector[String](), prog).mkString("\n")
  
    s"""|#include <stdio.h>
        |
        |int main(){
        |int tape[$init];
        |int p = 0;
        |
        |$methStr
        |}""".stripMargin
  }
}
