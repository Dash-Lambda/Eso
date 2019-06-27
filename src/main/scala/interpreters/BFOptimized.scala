package interpreters

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

case class BulkOp(ops: Vector[(Int, Int)], shift: Int){
  override def toString: String = s"BulkOp($shift, ${ops.mkString(" | ")})"
}

object BFOptimized extends Interpreter{
  def apply(log: Boolean, debug: Boolean)(progRaw: String): Try[String] = apply(1, -1, dynamicTapeSize = true, debug, log)(progRaw)
  def apply(initTapeSize: Int, outputMaxLength: Int, dynamicTapeSize: Boolean, log: Boolean, debug: Boolean)(progRaw: String): Try[String] = optimizeBulk(progRaw, debug) match{
    case Success((bops, prog)) => bfFunc(prog, bops, initTapeSize, outputMaxLength, dynamicTapeSize, debug, log)
    case Failure(e) => Failure(e)
  }
  
  def bfFunc(prog: Vector[(Char, Int)], bops: Vector[BulkOp], initTapeSize: Int, outputMaxLength: Int, dynamicTapeSize: Boolean, debug: Boolean, log: Boolean): Try[String] = {
    def printLog(str: String): String = {if(log) print(str); str}
  
    def scanRight(dat: Vector[Int], init: Int, stp: Int): Int = {
      if(stp == 1) dat.indexOf(0, init)
      else LazyList.from(init, stp).takeWhile(n => (dat.sizeIs > n) && (dat(n) != 0)).lastOption match{
        case Some(pos) => pos + stp
        case None => init
      }
    }
    def scanLeft(dat: Vector[Int], init: Int, stp: Int): Int = {
      if(stp == 1) dat.lastIndexOf(0, init)
      else LazyList.from(init, -stp).takeWhile(n => (n >= 0) && (dat(n) != 0)).lastOption match{
        case Some(pos) => pos - stp
        case None => init
      }
    }
  
    val updateBulk: (Vector[Int], Int, BulkOp) => Vector[Int] = (dat: Vector[Int], dc: Int, bop: BulkOp) => if(dynamicTapeSize){
      if(bop.ops.nonEmpty) bop.ops.foldLeft(dat.padTo(math.max(dc + bop.ops.last._1 + 1, dc + bop.shift + 1), 0)){case (vec, (ind, num)) => vec.updated(dc + ind, vec(dc + ind) + num)}
      else dat.padTo(dc + bop.shift + 1, 0)
    }else{
      if(bop.ops.nonEmpty) bop.ops.foldLeft(dat){case (vec, (ind, num)) => vec.updated(dc + ind, vec(dc + ind) + num)}
      else dat
    }
    val loopBulk: (Vector[Int], Int, BulkOp) => Vector[Int] = (dat: Vector[Int], dc: Int, bop: BulkOp) => if(dynamicTapeSize){
      if(dat(dc) != 0) bop.ops.foldLeft(dat.padTo(math.max(dc + bop.ops.last._1 + 1, dc + bop.shift + 1), 0)){case (vec, (ind, num)) => vec.updated(dc + ind, vec(dc + ind) + dat(dc)*num)}
      else dat.padTo(dc + bop.shift + 1, 0)
    }else{
      if(dat(dc) != 0) bop.ops.foldLeft(dat){case (vec, (ind, num)) => vec.updated(dc + ind, vec(dc + ind) + dat(dc)*num)}
      else dat
    }
    
    @tailrec
    def bfv(pc: Int, dc: Int, dat: Vector[Int], result: String): String = {
      if(debug) println(
        s"""|State: PC($pc)  DC($dc)
            |${dat.mkString(" | ")}""".stripMargin)
      
      Try{prog(pc)} match{
        case Success((op, num)) => op match{
          case 'u' =>
            val bop = bops(num)
            bfv(pc + 1, dc + bop.shift, updateBulk(dat, dc, bop), result)
          case 'l' =>
            val bop = bops(num)
            bfv(pc + 1, dc, loopBulk(dat, dc, bop), result)
          case '[' => bfv(if(dat(dc) == 0) num else pc + 1, dc, dat, result)
          case ']' => bfv(if(dat(dc) == 0) pc + 1 else num + 1, dc, dat, result)
          case ',' => bfv(pc + 1, dc, dat.updated(dc, StdIn.readInt), result)
          case '.' =>
            if((outputMaxLength == -1) || (result.sizeIs <= outputMaxLength - num)) bfv(pc + 1, dc, dat, result ++ printLog(dat(dc).toChar.toString * num))
            else result ++ printLog(dat(dc).toChar.toString * num)
          case '/' => bfv(pc + 1, scanRight(dat, dc, num), dat, result)
          case '\\' => bfv(pc + 1, scanLeft(dat, dc, num), dat, result)
          case '_' => bfv(pc + 1, dc, dat.updated(dc, 0), result)
        }
        case Failure(_) => result
      }
    }
  
    if(debug) println(
      s"""|Optimized: ${prog.map(_._1).mkString}
          |BulkOps: [${bops.mkString("], [")}]
          |Optimized Detail:
          |${prog.zipWithIndex.map{case ((c, n), ind) => s"$ind: $c $n\n"}.mkString}
          |""".stripMargin)
    
    Try{bfv(0, 0, Vector.fill(initTapeSize)(0), "")}
  }
  
  def optimizeBulk(progRaw: String, debug: Boolean): Try[(Vector[BulkOp], Vector[(Char, Int)])] = {
    def optBase(progSrc: String): Vector[(Char, Int)] = {
      val ops = Vector[Char]('>', '<', '+', '-', '.')
      val nonOps = Vector[Char]('[', ']', ',')
      
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
      
      bHelper(Vector[(Char, Int)](), ' ', 0, progSrc.filter("><][+-,.".contains(_)))
    }
    
    def optCS(progSrc: Vector[(Char, Int)]): Vector[(Char, Int)] = {
      @tailrec
      def cHelper(ac: Vector[(Char, Int)], src: Vector[(Char, Int)]): Vector[(Char, Int)] = {
        if(debug) println(
          s"""|Source: ${src.mkString(" | ")}
              |Opt: ${ac.mkString(" | ")}
              |""".stripMargin)
        src match{
          case ('[', _) +: ('-', 1) +: (']', _) +: tail => cHelper(ac :+ ('_', 1), tail)
          case ('[', _) +: ('>', n) +: (']', _) +: tail => cHelper(ac :+ ('/', n), tail)
          case ('[', _) +: ('<', n) +: (']', _) +: tail => cHelper(ac :+ ('\\', n), tail)
          case p +: ps => cHelper(ac :+ p, ps)
          case _ => ac
        }
      }
      
      cHelper(Vector[(Char, Int)](), progSrc)
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
            if(vac.nonEmpty || (shift != 0)) uHelper(ac :+ ('u', ind) :+ p, mac :+ BulkOp(vac.sortBy(_._1), shift), Vector[(Int, Int)](), 0, ind + 1, ps)
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
              sHelper(ac :+ ('[', ind), src.updated(ind - pos - 1, (']', pos)).tail)
            case Failure(e) => Failure(e)
          }
        case p +: ps => sHelper(ac :+ p, ps)
        case _ => Success(ac)
      }
      
      sHelper(Vector[(Char, Int)](), progSrc)
    }
    
    def counts(src: Vector[(Char, Int)]): String = s"[${src.count(_._1 == '[')}, ${src.count(_._1 == ']')}]"
    
    if(debug) println("Base optimization...\n")
    val pass0 = optBase(progRaw)
    if(debug) println(s"Clear/Scan Optimization...\n")
    val pass1 = optCS(pass0)
    if(debug) println(s"Bulk Optimization...\n")
    val (hmap, pass2) = optBulk(pass1)
    if(debug) println(s"Simple Loop Optimization...\n")
    val pass3 = optLoop(pass2, hmap)
    if(debug) println(s"Jump optimization...\n")
    val pass4 = optSkip(pass3)
    if(debug) println(
      s"""|
          |Base:  ${progRaw.filter("><+-[],.".contains(_))}
          |pass0: ${pass0.map(_._1).mkString}
          |Pass1: ${pass1.map(_._1).mkString}
          |Pass2: ${pass2.map(_._1).mkString}
          |Pass3: ${pass3.map(_._1).mkString}
          |Bracket Sums: [${progRaw.count(_ == '[')}, ${progRaw.count(_ == ']')}] -> ${counts(pass0)} -> ${counts(pass1)} -> ${counts(pass2)} -> ${counts(pass3)}""".stripMargin)
    
    pass4 match{
      case Success(prog) => Success(hmap, prog)
      case Failure(e) => Failure(e)
    }
  }
}
