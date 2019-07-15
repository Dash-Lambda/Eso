package interpreters

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

case class BulkOp(ops: Vector[(Int, Int)], shift: Int){
  override def toString: String = s"BulkOp($shift, ${ops.mkString(" | ")})"
}

object BFOptimizer {
  def apply(progRaw: String, debug: Boolean): Try[(Vector[BulkOp], Vector[(Char, Int)])] = {
    def optBase(progSrc: String): Vector[(Char, Int)] = {
      val ops = Vector[Char]('>', '<', '+', '-', '.')
      val nonOps = Vector[Char]('[', ']', ',', '_')
      
      @tailrec
      def bHelper(ac: Vector[(Char, Int)], tag: Char, count: Int, src: String): Vector[(Char, Int)] = {
        if(debug) println(
          s"""|Source: $src
              |Tag: $tag
              |Count: $count
              |Opt: ${ac.mkString(" | ")}
              |""".stripMargin)
        
        src.headOption match{
          case Some(c) =>
            if((tag == ' ') && ops.contains(c)) bHelper(ac, c, 1, src.tail)
            else if((tag == ' ') && nonOps.contains(c)) bHelper(ac :+ (c, 1), ' ', 0, src.tail)
            else if((tag != ' ') && (c == tag)) bHelper(ac, tag, count + 1, src.tail)
            else if((tag != ' ') && (c != tag) && ops.contains(c)) bHelper(ac :+ (tag, count), c, 1, src.tail)
            else bHelper(ac :+ (tag, count) :+ (c, 1), ' ', 0, src.tail)
          case None =>
            if(tag == ' ') ac
            else ac :+ (tag, count)
        }
      }
  
      val conditioned = progSrc.filter("><][+-,.".contains(_))
      bHelper(Vector[(Char, Int)](), ' ', 0, conditioned) :+ ('e', 0)
    }
    
    def optBulk(progSrc: Vector[(Char, Int)]): (Vector[BulkOp], Vector[(Char, Int)]) = {
      @tailrec
      def uHelper(ac: Vector[(Char, Int)], mac: Vector[BulkOp], vac: Vector[(Int, Int)], shift: Int, ind: Int, src: Vector[(Char, Int)]): (Vector[BulkOp], Vector[(Char, Int)]) = {
        if(debug) println(
          s"""|Source: ${src.mkString(" | ")}
              |BulkOps: ${vac.mkString(" | ")}
              |Shift: $shift
              |Addr: $ind
              |Opt: ${ac.mkString(" | ")}
              |""".stripMargin)
        
        src match{
          case ('>', n) +: tail => uHelper(ac, mac, vac, shift + n, ind, tail)
          case ('<', n) +: tail => uHelper(ac, mac, vac, shift - n, ind, tail)
          case ('+', n) +: tail => uHelper(ac, mac, vac :+ (shift, n), shift, ind, tail)
          case ('-', n) +: tail => uHelper(ac, mac, vac :+ (shift, -n), shift, ind, tail)
          case p +: ps =>
            if(vac.sizeIs == 1) uHelper(ac :+ ('a', ind) :+ p, mac :+ BulkOp(vac, shift), Vector[(Int, Int)](), 0, ind + 1, ps)
            else if(vac.nonEmpty) uHelper(ac :+ ('u', ind) :+ p, mac :+ BulkOp(vac.sortBy(_._1), shift), Vector[(Int, Int)](), 0, ind + 1, ps)
            else if(vac.isEmpty && shift != 0) uHelper(ac :+ ('m', shift) :+ p, mac, Vector[(Int, Int)](), 0, ind, ps)
            else uHelper(ac :+ p, mac, vac, shift, ind, ps)
          case _ => (mac, ac)
        }
      }
      
      uHelper(Vector[(Char, Int)](), Vector[BulkOp](), Vector[(Int, Int)](), 0, 0, progSrc)
    }
    
    def optLoop(progSrc: Vector[(Char, Int)], hmap: Vector[BulkOp]): Vector[(Char, Int)] = {
      @tailrec
      def lHelper(ac: Vector[(Char, Int)], src: Vector[(Char, Int)]): Vector[(Char, Int)] = {
        if(debug) println(
          s"""|Source: ${src.mkString(" | ")}
              |Opt: ${ac.mkString(" | ")}
              |""".stripMargin)
        
        src match{
          case ('[', _) +: ('u', addr) +: (']', _) +: tail if hmap(addr).shift == 0 && hmap(addr).ops.filter(_._1 == 0).map(_._2).sum == -1 => lHelper(ac :+ ('l', addr), tail)
          case ('[', _) +: ('m', stp) +: (']', _) +: tail => lHelper(ac :+ ('/', stp), tail)
          case p +: ps => lHelper(ac :+ p, ps)
          case _ => ac
        }
      }
      
      lHelper(Vector[(Char, Int)](), progSrc)
    }
    
    def optSkip(progSrc: Vector[(Char, Int)]): Try[Vector[(Char, Int)]] = {
      @tailrec
      def scrub(ind: Int, count: Int): Try[Int] = {
        if(progSrc.isDefinedAt(ind)){
          (progSrc(ind)._1, count) match{
            case (']', 0) => Success(ind + 1)
            case ('[', _) => scrub(ind + 1, count + 1)
            case (']', _) => scrub(ind + 1, count - 1)
            case _ => scrub(ind + 1, count)
          }
        } else Failure(InterpreterException("Bracket Mismatch"))
      }
      
      @tailrec
      def sHelper(ac: Vector[(Char, Int)], src: Vector[(Char, Int)]): Try[Vector[(Char, Int)]] = src match{
        case ('[', _) +: _ =>
          val pos = ac.length
          scrub(pos + 1, 0) match{
            case Success(ind) =>
              if(debug) println(s"Skip: $pos -> $ind")
              sHelper(ac :+ ('[', ind), src.updated(ind - pos - 1, (']', pos + 1)).tail)
            case Failure(e) => Failure(e)
          }
        case p +: ps => sHelper(ac :+ p, ps)
        case _ => Success(ac)
      }
      
      sHelper(Vector[(Char, Int)](), progSrc)
    }
    
    def counts(src: Vector[(Char, Int)]): String = s"[${src.count(_._1 == '[')}, ${src.count(_._1 == ']')}]"
    
    if(debug) println("Base optimization...\n")
    val pass1 = optBase(progRaw)
    if(debug) println(s"Bulk Optimization...\n")
    val (hmap, pass2) = optBulk(pass1)
    if(debug) println(s"Simple Loop Optimization...\n")
    val pass3 = optLoop(pass2, hmap)
    if(debug) println(s"Jump optimization...\n")
    val pass4 = optSkip(pass3)
    if(debug) println(
      s"""|
          |Base:  ${progRaw.filter("><+-[],.".contains(_))}
          |Pass1: ${pass1.map(_._1).mkString}
          |Pass2: ${pass2.map(_._1).mkString}
          |Pass3: ${pass3.map(_._1).mkString}
          |Bracket Sums: [${progRaw.count(_ == '[')}, ${progRaw.count(_ == ']')}] -> ${counts(pass1)} -> ${counts(pass2)} -> ${counts(pass3)}""".stripMargin)
    
    pass4 match{
      case Success(prog) => Success(hmap, prog)
      case Failure(e) => Failure(e)
    }
  }
}
