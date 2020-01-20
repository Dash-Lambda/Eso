package brainfuck

import scala.annotation.tailrec

object BFToScala extends BFTranspiler{
  val dst: String = "Scala"
  
  def genProg(init: Int, olen: Int, dyn: Boolean, methSize: Int, prog: Vector[BFOp]): String = {
    @tailrec
    def tdo(ac: Vector[String] = Vector(), stk: Vector[Vector[String]] = Vector(), tmp: Vector[String] = Vector("def f0(): Unit = {"), fnum: Int = 1, i: Int = 0): String = prog.lift(i) match{
      case Some(op) => op match{
        case BFOpenLoop(_) => tdo(ac, (tmp :+ s"f$fnum()") +: stk, Vector(s"def f$fnum(): Unit = while(${if(dyn) "p < len && " else ""}tape(p) != 0){"), fnum + 1, i + 1)
        case BFCloseLoop(_) => tdo(segScala(tmp :+ "}", methSize).mkString("\n") +: ac, stk.tail, stk.head, fnum, i + 1)
        case BFEnd => (segScala(tmp :+ "}", methSize).mkString("\n") +: ac).mkString("\n")
        case _ =>
          val block = op match{
            case bop: SingOp => s"${if(dyn) s"chkInd(${bop.maxShift})\n" else ""}${opStr(bop)}"
            case bop: LoopOp => s"${if(dyn) s"chkInd(${bop.maxShift})\n" else ""}${lopStr(bop)}"
            case BFMove(n) => s"p ${incStr(n)}"
            case BFScan(n) =>
              if (n == 1) "p = tape.indexOf(0, p)"
              else if (n == -1) "p = tape.lastIndexOf(0, p)"
              else if(dyn) s"while(p < len && tape(p) != 0){p ${incStr(n)}}"
              else s"while(tape(p) != 0){p ${incStr(n)}}"
            case BFIn => "tape(p) = inp.head.toInt\ninp = inp.tail"
            case BFOut => "print(tape(p).toChar)"}
          val block2 = if(dyn && "m/".contains(op)) s"$block\nchkInd()" else block
          tdo(ac, stk, tmp :+ block2, fnum, i + 1)}
      case _ => ac.mkString("\n\n")}
    
    val dynFunc: String =
      """|
         |def chkInd(shift: Int = 0): Unit = {
         |if(p == -1){p = len; len += 1; tape = tape.padTo(len, 0)}
         |else if(p + shift >= len){len = p + shift + 1; tape = tape.padTo(len, 0)}
         |}
         |""".stripMargin
    
    s"""|object BFProg{
        |var tape = Array[Int]()${if (dyn) "\nvar len = 0" else ""}
        |var p = 0
        |var inp = LazyList[Char]()${if (dyn) dynFunc else ""}
        |def main(args: Array[String]): Unit = {
        |tape = Array.fill($init)(0)${if(dyn) s"\nlen = $init" else ""}
        |p = 0
        |inp = LazyList.continually(scala.io.StdIn.readLine()).map(_ + "\\n").flatten
        |f0()
        |}
        |${tdo()}
        |}""".stripMargin}
}
