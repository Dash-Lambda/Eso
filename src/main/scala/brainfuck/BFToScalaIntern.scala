package brainfuck

import scala.annotation.tailrec

object BFToScalaIntern extends BFTranspiler{
  val dst: String = "Scala"
  
  def genProg(init: Int, olen: Int, dyn: Boolean, methSize: Int, prog: Vector[(Char, Either[Int, BlkOp])]): String = {
    @tailrec
    def tdo(ac: Vector[String] = Vector(), stk: Vector[Vector[String]] = Vector(), tmp: Vector[String] = Vector("def f0(): Unit = {"), fnum: Int = 1, i: Int = 0): String = prog.lift(i) match{
      case Some((op, arg)) => op match{
        case '[' =>
          val call = s"f$fnum()${if(olen >= 0) "\nif(end) return ()" else ""}"
          val sig = s"def f$fnum(): Unit = while(tape(p) != 0){"
          tdo(ac, (tmp :+ call) +: stk, Vector(sig), fnum + 1, i + 1)
        case ']' => tdo(segScala(tmp :+ "}", methSize).mkString("\n") +: ac, stk.tail, stk.head, fnum, i + 1)
        case 'e' => (segScala(tmp :+ "}", methSize).mkString("\n") +: ac).mkString("\n")
        case _ =>
          val block = arg match{
            case Right(bop) => op match{
              case 'u' | 'a' => s"${if(dyn) s"chkInd(${bop.maxShift})\n" else ""}${opStr(bop)}"
              case 'l' => s"${if(dyn) s"chkInd(${bop.maxShift})\n" else ""}${lopStr(bop)}"}
            case Left(num) => op match{
              case 'm' => s"p ${incStr(num)}"
              case '/' =>
                if (num == 1) "p = tape.indexOf(0, p)"
                else if (num == -1) "p = tape.lastIndexOf(0, p)"
                else if(dyn) s"while(p < len && tape(p) != 0){p ${incStr(num)}}"
                else s"while(tape(p) != 0){p ${incStr(num)}}"
              case ',' => "tape(p) = inp.head.toInt\ninp = inp.tail"
              case '.' =>
                val limStr =
                  s"""|
                      |resLen += 1
                      |if(resLen >= $olen){end = true; return ()}""".stripMargin
                s"queue.put(Some(Success(tape(p).toChar)))${if(olen >= 0) limStr else ""}"}}
          val block2 = if(dyn && "m/".contains(op)) s"$block\nchkInd()" else block
          tdo(ac, stk, tmp :+ block2, fnum, i + 1)}
      case _ => ac.mkString("\n")}
    
    val dynFunc: String =
      s"""|
          |def chkInd(shift: Int = 0): Unit = {
          |if(p == -1){p = len; len += 1; tape = tape.padTo(len, 0)}
          |else if(p + shift >= len){len = p + shift + 1; tape = tape.padTo(len, 0)}
          |}
          |""".stripMargin
    
    val methStr = tdo()
    
    s"""|import java.util.concurrent.{BlockingQueue, SynchronousQueue}
        |import scala.util.{Try, Success, Failure}
        |new Function2[BlockingQueue[Option[Try[Char]]], Seq[Char], Runnable]{
        |def apply(queue: BlockingQueue[Option[Try[Char]]], inputs: Seq[Char]): Runnable = Stepper(queue, inputs)
        |case class Stepper(queue: BlockingQueue[Option[Try[Char]]], inputs: Seq[Char]) extends Runnable{
        |var tape = Array[Int]()${if(dyn) s"\nvar len = 0" else ""}
        |var p = 0
        |var inp = Seq[Char]()${if(olen >= 0) s"\nvar resLen = 0\nvar end = false" else ""}${if(dyn) dynFunc else ""}
        |def run(): Unit = {
        |inp = inputs
        |tape = Array.fill($init)(0)${if(dyn) s"\nlen = $init" else ""}
        |p = 0
        |try{
        |f0()
        |queue.put(None)}
        |catch{
        |case e: Throwable => queue.put(Some(Failure(e)))
        |}
        |}
        |$methStr
        |}
        |}""".stripMargin}
}
