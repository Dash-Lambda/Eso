package brainfuck

import scala.annotation.tailrec

object BFToScala extends BFTranspiler{
  val dst: String = "Scala"
  
  def genProg(init: Int, olen: Int, dyn: Boolean, methSize: Int, prog: LazyList[(Char, Either[Int, BlkOp])]): String = {
    def incStr(n: Int): String = s"${if(n < 0) "-" else "+"}= ${n.abs}"
    
    @tailrec
    def cgo(ac: Vector[String], src: LazyList[(Char, Either[Int, BlkOp])]): Vector[String] = {
      src match {
        case (op, arg) +: ops =>
          val str = arg match {
            case Left(num) => op match {
              case 'm' => s"p ${incStr(num)}"
              case '/' =>
                if (num == 1) s"p = tape.indexOf(0, p)"
                else if (num == -1) s"p = tape.lastIndexOf(0, p)"
                else s"while(${if(dyn) "p < len && " else ""}tape(p) != 0){p ${incStr(num)}}"
              case '[' => "while(tape(p) != 0){"
              case ']' => "}"
              case ',' => "tape(p) = inp.head.toInt\ninp = inp.tail"
              case '.' => "print(tape(p).toChar)"
              case 'e' => ""
            }
            case Right(bop) => op match {
              case 'u' | 'a' => s"${if (dyn) s"chkInd(${bop.maxShift})\n" else ""}${bop.opStr}"
              case 'l' => s"${if (dyn) s"chkInd(${bop.maxShift})\n" else ""}${bop.lopStr}"
            }
          }
          cgo(ac :+ str, ops)
        case _ => ac
      }
    }
  
    val dynFunc: String =
      s"""|
          |def chkInd(shift: Int = 0): Unit = {
          |  if(p == -1){p = len; len += 1; tape = tape.padTo(len, 0)}
          |  else if(p + shift >= len){len = p + shift + 1; tape = tape.padTo(len, 0)}
          |}
          |""".stripMargin
  
    val methStr = cgo(Vector[String](), prog).mkString("\n")
  
    s"""|var tape = Array.fill($init)(0)${if (dyn) s"\nvar len = 0" else ""}
        |var p = 0
        |var inp = LazyList.continually(scala.io.StdIn.readLine()).flatten${if (olen >= 0) s"\nvar resLen = 0\nvar end = false" else ""}
        |${if (dyn) dynFunc else ""}
        |$methStr""".stripMargin
  }
}
