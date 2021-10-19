package languages.brainfuck

import scala.annotation.tailrec

object BFToScalaIntern extends BFTranspiler{
  val dst: String = "Scala"

  def genProg(init: Int, olen: Int, dyn: Boolean, methSize: Int, prog: Vector[BFOp]): String = {
    def segment(blocks: Vector[String], fnum: Int): Vector[String] = {
      blocks.grouped(methSize).toVector match{
        case bs :+ e => bs.zipWithIndex.flatMap{case (b: Vector[String], i) => b :+ s"tailcall(f${fnum}s$i())\n}\ndef f${fnum}s$i(): TailRec[Option[Step]] = {"} :++ e :+ "}"}}

    def dynCheck(shift: Int): String =
      s"""if(p == -1){p = len; len += 1; tape = tape.padTo(len, 0)} else if(${if(shift == 0) "p" else s"p + $shift"} >= len){len = ${if(shift == -1) "p" else s"p + ${shift + 1}"}; tape = tape.padTo(len, 0)}"""

    def makeScala(progRaw: Vector[BFOp]): String = {
      @tailrec
      def tdo(ac: Vector[String], stk: Vector[Vector[String]], tmp: Vector[String], loops: Vector[Int], fnum: Int, i: Int): String = {
        progRaw.lift(i) match{
          case Some(op) => op match{
            case BFOpenLoop(1) =>
              val call = s"tailcall(f$fnum())"
              val sig = s"def f$fnum(): TailRec[Option[Step]] = if(${if(dyn) "p < len && " else ""}tape(p) == 0) tailcall(f${fnum}a()) else {"
              tdo(ac :++ segment(tmp :+ call, fnum), stk, Vector(sig), fnum +: loops, fnum + 1, i + 1)
            case BFOpenLoop(0) =>
              val call = s"f$fnum()"
              val sig = s"def f$fnum(): Unit = while(${if(dyn) "p < len && " else ""}tape(p) != 0){"
              tdo(ac, (tmp :+ call) +: stk, Vector(sig), loops, fnum + 1, i + 1)
            case BFCloseLoop(1) => tdo(ac :++ segment(tmp :+ s"tailcall(f${loops.head}())", fnum), stk, Vector(s"def f${loops.head}a(): TailRec[Option[Step]] = {"), loops.tail, fnum, i + 1)
            case BFCloseLoop(0) => tdo(ac :++ segScala(tmp :+ "}", methSize), stk.tail, stk.head, loops, fnum, i + 1)
            case BFEnd => (ac :++ segment(tmp :+ "done(None)", fnum)).mkString("\n")
            case BFOut(n) => progRaw.lift(i + 1) match{
              case Some(BFCloseLoop(1)) =>
                val blk = s"done(Some(Step(tape(p).toChar.toString${if(n == 1) "" else s"*$n"}, tailcall(f${loops.head}()))))"
                tdo(ac :++ segment(tmp :+ blk, fnum), stk, Vector(s"def f${loops.head}a(): TailRec[Option[Step]] = {"), loops.tail, fnum, i + 2)
              case Some(BFEnd) => (ac :++ segment(tmp :+ s"done(Some(Step(tape(p).toChar.toString${if(n == 1) "" else s"*$n"}, done(None))))", fnum)).mkString("\n")
              case _ =>
                val blk = s"done(Some(Step(tape(p).toChar.toString${if(n == 1) "" else s"*$n"}, tailcall(f$fnum()))))"
                tdo(ac :++ segment(tmp :+ blk, fnum), stk, Vector(s"def f$fnum(): TailRec[Option[Step]] = {"), loops, fnum + 1, i + 1)}
            case _ =>
              val block = op match{
                case bop: SingOp => s"${if(dyn) s"${dynCheck(bop.maxShift)}\n" else ""}${opStr(bop)}"
                case bop: LoopOp => s"${if(dyn) s"${dynCheck(bop.maxShift)}\n" else ""}${lopStr(bop)}"
                case BFMove(n) => s"p ${incStr(n)}"
                case BFScan(n) =>
                  if (n == 1) "p = tape.indexOf(0, p)"
                  else if (n == -1) "p = tape.lastIndexOf(0, p)"
                  else if(dyn) s"while(p < len && tape(p) != 0){p ${incStr(n)}}"
                  else s"while(tape(p) != 0){p ${incStr(n)}}"
                case BFIn => "tape(p) = inp.head.toInt\ninp = inp.tail"}
              val block2 = if(dyn && "m/".contains(op)) s"$block\nchkInd()" else block
              tdo(ac, stk, tmp :+ block2, loops, fnum, i + 1)}
          case _ => ac.mkString("\n")}}
      tdo(Vector(), Vector(), Vector("def f0(): TailRec[Option[Step]] = {"), Vector(), 1, 0)}

    val methStr = makeScala(prog)

    val finStr =
      s"""|new Function1[Seq[Char], LazyList[Char]]{
          |  import scala.util.control.TailCalls.{TailRec, tailcall, done}
          |  var tape = Array[Int]()
          |  var p = 0
          |  var inp = Seq[Char]()
          |  ${if (dyn) s"\nvar len = 0" else ""}
          |  ${if (olen >= 0) s"\nvar resLen = 0" else ""}
          |  case class Step(str: String, nxt: TailRec[Option[Step]]){def resolve(): (String, TailRec[Option[Step]]) = (str, nxt)}
          |  def apply(inputs: Seq[Char]): LazyList[Char] = {
          |    inp = inputs
          |    tape = Array.fill($init)(0)
          |    p = 0
          |    LazyList.unfold(tailcall(f0()))(nxt => nxt.result.map(_.resolve)).flatten
          |  }
          |  $methStr
          |}""".stripMargin
    //println(s"\n\n${indent(finStr)}\n\n")
    finStr
  }
}
