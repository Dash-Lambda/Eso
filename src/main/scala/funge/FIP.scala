package funge

import java.util.Calendar

import common.Vec2D

import spire.implicits._

import scala.collection.immutable

trait FIPRet{
  def step: FIPRet
}
case class FIPCont(prog: BF98Prog, inp: Seq[Char], fip: FIP) extends FIPRet{
  def step: FIPCont = FIPCont(prog, inp, fip.step(prog))
}
case class FIPSplit(prog: BF98Prog, inp: Seq[Char], fip1: FIP, fip2: FIP) extends FIPRet{
  def step: FIPSplit = FIPSplit(prog: BF98Prog, inp, fip1.step(prog), fip2.step(prog))
}
case class FIPOut(out: String, prog: BF98Prog, inp: Seq[Char], fip: FIP) extends FIPRet{
  def step: FIPOut = FIPOut(out, prog, inp, fip.step(prog))
}
case class FIPHalt(code: Option[Int]) extends FIPRet{
  def step: FIPHalt = this
}

case class FungeStack(vec: Vector[Int]){
  def apply(i: Int): Int = vec.lift(i).getOrElse(0)
  
  def take(n: Int): Vector[Int] = vec.take(n).padTo(n, 0)
  def drop(n: Int): FungeStack = FungeStack(vec.drop(n))
  
  def head: Int = vec.headOption.getOrElse(0)
  def tail: FungeStack = FungeStack(vec.drop(1))
  
  def :++(seq: Seq[Int]): FungeStack = FungeStack(vec :++ seq)
  def ++:(seq: Seq[Int]): FungeStack = FungeStack(seq ++: vec)
  
  def +:(n: Int): FungeStack = FungeStack(n +: vec)
  def :+(n: Int): FungeStack = FungeStack(vec :+ n)
  
  def size: Int = vec.size
}
object #-:{
  def unapply(stk: FungeStack): Option[(Int, FungeStack)] = Some((stk.head, stk.tail))
}

case class FIP(id: Int, ip: Vec2D[Int], dt: Vec2D[Int], so: Vec2D[Int], bs: Boolean, stk: Vector[FungeStack], binds: immutable.HashMap[Char, Vector[(BF98Prog, Seq[Char], FIP) => FIPRet]]){
  def apply(prog: BF98Prog, inp: Seq[Char]): FIPRet = {
    val npos = prog.getNextInd(ip, dt)
    val op = prog(ip).toChar
    doOp(prog, inp, npos, op)}
  
  def step(prog: BF98Prog): FIP = FIP(id, prog.getNextInd(ip, dt), dt, so, bs, stk, binds)
  
  def doOp(prog: BF98Prog, inp: Seq[Char], npos: Vec2D[Int], op: Char): FIPRet = {
    if(bs){
      if(op == '"') FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, dt), dt, so, bs=false, stk, binds))
      else if(op == ' ') FIPCont(prog, inp, FIP(id, prog.skipAll(ip, dt, op.toInt), dt, so, bs, (op.toInt +: TOSS) +: stk.tail, binds))
      else FIPCont(prog, inp, FIP(id, ip + dt, dt, so, bs, (op.toInt +: TOSS) +: stk.tail, binds))}
    else{
      if(op != 'k') exec(prog, inp, npos, op)
      else TOSS match{
        case n #-: ns =>
          if(n == 0){
            val npos2 = prog.getNextInd(npos, dt)
            FIPCont(prog, inp, FIP(id, npos2, dt, so, bs, ns +: stk.tail, binds))}
          else{
            prog(npos).toChar match{
              case '#' => FIPCont(prog, inp, FIP(id, prog.getNextInd(ip + (dt*n), dt), dt, so, bs, ns +: stk.tail, binds))
              case op2 => rep(prog, inp, ip, op2, n).step}}}}}
  
  def rep(prog: BF98Prog, inp: Seq[Char], npos: Vec2D[Int], opChar: Char, num: Int): FIPRet = {
    lazy val opVec = Vector.fill(num)(opChar)
    lazy val res = opVec.foldLeft(("": String, FIP(id, npos, dt, so, bs, TOSS.tail +: stk.tail, binds), prog, inp)){
      case ((str, fip, prg, in), op) =>
        fip.doOp(prg, in, fip.ip, op) match{
          case FIPCont(nPrg, nIn, nFip) => (str, nFip, nPrg, nIn)
          case FIPOut(nStr, nPrg, nIn, nFip) => (str ++ nStr, nFip, nPrg, nIn)}}
    
    opChar match{
      case '@' | 'q' => exec(prog, inp, npos, opChar)
      case _ => res match{
        case (str, fip, prg, in) =>
          if(str.isEmpty) FIPCont(prg, in, fip)
          else FIPOut(str, prg, in, fip)}}}
  
  def exec(prog: BF98Prog, inp: Seq[Char], npos: Vec2D[Int], op: Char): FIPRet = {
    op match{
      case '!' =>
        val nStk = ((if(TOSS.head == 0) 1 else 0) +: TOSS.tail) +: stk.tail
        FIPCont(prog, inp, FIP(id, npos, dt, so, bs, nStk, binds))
      case '\"' => FIPCont(prog, inp, FIP(id, ip + dt, dt, so, bs=true, stk, binds))
      case '#' =>  FIPCont(prog, inp, FIP(id, prog.getNextInd(ip + dt, dt), dt, so, bs, stk, binds))
      case '$' =>
        val nStk = TOSS.tail +: stk.tail
        FIPCont(prog, inp, FIP(id, npos, dt, so, bs, nStk, binds))
      case '&' => chompNum(inp) match{
        case (num, ninp) => FIPCont(prog, ninp, FIP(id, npos, dt, so, bs, (num +: TOSS) +: stk.tail, binds))}
      case '\'' => FIPCont(prog, inp, FIP(id, prog.getNextInd(ip + dt, dt), dt, so, bs, (prog(ip + dt) +: TOSS) +: stk.tail, binds))
      case 's' => TOSS match{
        case v #-: ns => FIPCont(prog.updated(ip + dt, v), inp, FIP(id, prog.getNextInd(ip + dt, dt), dt, so, bs, ns +: stk.tail, binds))}
      case '*' => TOSS match{
        case b #-: a #-: ns => FIPCont(prog, inp, FIP(id, npos, dt, so, bs, ((a*b) +: ns) +: stk.tail, binds))}
      case '/' => TOSS match{
        case b #-: a #-: ns =>
          val quot = if(b == 0 && prog.bDiv) 0 else a/b
          FIPCont(prog, inp, FIP(id, npos, dt, so, bs, (quot +: ns) +: stk.tail, binds))}
      case '%' =>TOSS match{
        case b #-: a #-: ns =>
          val rem = if(b == 0 && prog.bDiv) 0 else a%b
          FIPCont(prog, inp, FIP(id, npos, dt, so, bs, (rem +: ns) +: stk.tail, binds))}
      case '+' => TOSS match{
        case b #-: a #-: ns => FIPCont(prog, inp, FIP(id, npos, dt, so, bs, ((a + b) +: ns) +: stk.tail, binds))}
      case '-' => TOSS match{
        case b #-: a #-: ns => FIPCont(prog, inp, FIP(id, npos, dt, so, bs, ((a - b) +: ns) +: stk.tail, binds))}
      case '.' => FIPOut(TOSS.head.toString + ' ', prog, inp, FIP(id, npos, dt, so, bs, TOSS.tail +: stk.tail, binds))
      case ',' => FIPOut(TOSS.head.toChar.toString, prog, inp, FIP(id, npos, dt, so, bs, TOSS.tail +: stk.tail, binds))
      case n if n.isDigit => FIPCont(prog, inp, FIP(id, npos, dt, so, bs, (n.asDigit +: TOSS) +: stk.tail, binds))
      case c if Range('a', 'g').contains(c) => FIPCont(prog, inp, FIP(id, npos, dt, so, bs, ((c.toInt - 87) +: TOSS) +: stk.tail, binds))
      case ':' => FIPCont(prog, inp, FIP(id, npos, dt, so, bs, (TOSS.head +: TOSS.head +: TOSS.tail) +: stk.tail, binds))
      case '<' =>
        val nDt = Vec2D(-1, 0)
        FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, nDt), nDt, so, bs, stk, binds))
      case '>' =>
        val nDt = Vec2D(1, 0)
        FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, nDt), nDt, so, bs, stk, binds))
      case '^' =>
        val nDt = Vec2D(0, -1)
        FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, nDt), nDt, so, bs, stk, binds))
      case 'v' =>
        val nDt = Vec2D(0, 1)
        FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, nDt), nDt, so, bs, stk, binds))
      case '?' =>
        val nDt = prog.rand.nextInt(4) match{
          case 0 => Vec2D(-1, 0)
          case 1 => Vec2D(1, 0)
          case 2 => Vec2D(0, -1)
          case 3 => Vec2D(0, 1)}
        FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, nDt), nDt, so, bs, stk, binds))
      case '[' =>
        val nDt = Vec2D(dt.y, -dt.x)
        FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, nDt), nDt, so, bs, stk, binds))
      case ']' =>
        val nDt = Vec2D(-dt.y, dt.x)
        FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, nDt), nDt, so, bs, stk, binds))
      case '@' => FIPHalt(None)
      case 'q' => FIPHalt(Some(TOSS.head))
      case '\\' => TOSS match{
        case b #-: a #-: ns => FIPCont(prog, inp, FIP(id, npos, dt, so, bs, (a +: b +: ns) +: stk.tail, binds))}
      case '_' =>
        val nDt = if(TOSS.head == 0) Vec2D(1, 0) else Vec2D(-1, 0)
        FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, nDt), nDt, so, bs, TOSS.tail +: stk.tail, binds))
      case '|' =>
        val nDt = if(TOSS.head == 0) Vec2D(0, 1) else Vec2D(0, -1)
        FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, nDt), nDt, so, bs, TOSS.tail +: stk.tail, binds))
      case '`' => TOSS match{
        case b #-: a #-: ns =>
          val gt = if(a > b) 1 else 0
          FIPCont(prog, inp, FIP(id, npos, dt, so, bs, (gt +: ns) +: stk.tail, binds))}
      case 'g' => TOSS match{
        case y #-: x #-: ns => FIPCont(prog, inp, FIP(id, npos, dt, so, bs, (prog(so + Vec2D(x, y)) +: ns) +: stk.tail, binds))}
      case 'p' => TOSS match{
        case y #-: x #-: v #-: ns => FIPCont(prog.updated(so + Vec2D(x, y), v), inp, FIP(id, npos, dt, so, bs, ns +: stk.tail, binds))}
      case 'j' => TOSS match{
        case n #-: ns => FIPCont(prog, inp, FIP(id, prog.getNextInd(ip + (dt*n), dt), dt, so, bs, ns +: stk.tail, binds))}
      case 'n' => FIPCont(prog, inp, FIP(id, npos, dt, so, bs, FungeStack(Vector()) +: stk.tail, binds))
      case 'r' => FIPCont(prog, inp, FIP(id, npos, -dt, so, bs, stk, binds))
      case 't' => FIPSplit(prog, inp, FIP(id, npos, dt, so, bs, stk, binds), FIP(id, prog.getNextInd(ip, -dt), -dt, so, bs, stk, binds))
      case 'u' =>
        if(stk.sizeIs < 2) FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, -dt), -dt, so, bs, stk, binds))
        else TOSS match{
          case n #-: ns =>
            if(n == 0) FIPCont(prog, inp, FIP(id, npos, dt, so, bs, ns +: stk.tail, binds))
            else if(n > 0) FIPCont(prog, inp, FIP(id, npos, dt, so, bs, (SOSS.take(n).reverse ++: ns) +: SOSS.drop(n) +: stk.drop(2), binds))
            else FIPCont(prog, inp, FIP(id, npos, dt, so, bs, ns.drop(-n) +: (ns.take(-n).reverse ++: SOSS) +: stk.drop(2), binds))}
      case 'w' => TOSS match{
        case a #-: b #-: ns =>
          val nDt = if(a > b) Vec2D(dt.y, -dt.x) else if(a < b) Vec2D(-dt.y, dt.x) else dt
          FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, nDt), nDt, so, bs, ns +: stk.tail, binds))}
      case 'x' => TOSS match{
        case y #-: x #-: ns =>
          val nDt = Vec2D(x, y)
          FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, nDt), nDt, so, bs, ns +: stk.tail, binds))}
      case 'z' => FIPCont(prog, inp, FIP(id, npos, dt, so, bs, stk, binds))
      case '{' => TOSS match{
        case n #-: ns =>
          val num = math.max(0, n)
          val nStk = (ns.take(num) ++: FungeStack(Vector())) +: (so.y +: so.x +: LazyList.fill(math.max(0, -n))(0) ++: ns.drop(num)) +: stk.drop(2)
          FIPCont(prog, inp, FIP(id, npos, dt, npos, bs, nStk, binds))}
      case '}' => stk match{
        case (n #-: ns) +: (y #-: x #-: ss) +: tail =>
          val nStk = if(n >= 0) (ns.take(n) ++: ss) +: tail else ss.drop(n.abs) +: tail
          FIPCont(prog, inp, FIP(id, npos, dt, Vec2D(x, y), bs, nStk, binds))
        case _ => FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, -dt), -dt, so, bs, stk, binds))}
      case '~' => FIPCont(prog, inp.tail, FIP(id, npos, dt, so, bs, (inp.head.toInt +: TOSS) +: stk.tail, binds))
      case 'y' =>
        import SysInf._
        val (low, high) = prog.getBounds
        val cal = prog.cal
        val date = cal.get(Calendar.DATE)
        val year = cal.get(Calendar.YEAR)
        val month = cal.get(Calendar.MONTH) + 1
        val hour = cal.get(Calendar.HOUR)
        val minute = cal.get(Calendar.MINUTE)
        val second = cal.get(Calendar.SECOND)
        val ymd = ((year - 1900)*256*256) + (month*256) + date
        val hms = (hour*256*256) + (minute*256) + second
        
        val flags = Vector(tFlg, iFlg, oFlg, eqFlg, bufFlg).zipWithIndex.foldLeft(0: Int){case (s, (f, n)) => s + (f*(2**n))}
        val global = Vector(flags, bytesPerCell, handPrint, version, paradigm, pathSep.toInt, dims)
        val local = Vector(id, 0, ip.y, ip.x, dt.y, dt.x, so.y, so.x)
        val env = Vector(low.y, low.x, high.y, high.x, ymd, hms)
        val stkInf = stk.size +: TOSS.tail.size +: stk.tail.map(_.size)
        val args = Vector.fill(4)(0)
        
        val infoStk = (global ++ local ++ env ++ stkInf ++ args).to(LazyList)
        TOSS match{
          case n #-: ns =>
            if(n <= 0) FIPCont(prog, inp, FIP(id, npos, dt, so, bs, (infoStk ++: ns) +: stk.tail, binds))
            else{
              val tmpToss = infoStk ++: ns
              val pick = tmpToss(n - 1)
              FIPCont(prog, inp, FIP(id, npos, dt, so, bs, (pick +: ns) +: stk.tail, binds))}}
      case c if binds.isDefinedAt(c) && binds(c).nonEmpty => binds(c).head(prog, inp, this)
      case '(' => TOSS match{
        case n #-: ns =>
          val id = ns.take(n).foldLeft(0){case (ac, m) => (ac*256) + m}
          BF98Lib.get(id) match{
            case Some(fp) =>
              val nBind = fp.binds.foldLeft(binds){
                case (bnds, (k, b)) =>
                  val eb = bnds.get(k) match{
                    case Some(v) => v
                    case None => Vector()}
                  bnds + ((k, b +: eb))}
              FIPCont(prog, inp, FIP(id, npos, dt, so, bs, (1 +: id +:ns.drop(n)) +: stk.tail, nBind))
            case None => FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, -dt), -dt, so, bs, ns.drop(n) +: stk.tail, binds))}}
      case ')' => TOSS match{
        case n #-: ns =>
          val id = ns.take(n).foldLeft(0){case (ac, m) => (ac*256) + m}
          BF98Lib.get(id) match{
            case Some(fp) =>
              val nBind = fp.binds.foldLeft(binds){
                case (bnds, (k, _)) => bnds.get(k) match{
                  case Some(v) => bnds + ((k, v.drop(1)))
                  case None => bnds}}
              FIPCont(prog, inp, FIP(id, npos, dt, so, bs, ns.drop(n) +: stk.tail, nBind))
            case None => FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, -dt), -dt, so, bs, ns.drop(n) +: stk.tail, binds))}}
      case 'i' => FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, -dt), -dt, so, bs, stk, binds)) //Input file (not implemented)
      case 'o' => FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, -dt), -dt, so, bs, stk, binds)) //Output file (not implemented)
      case '=' => FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, -dt), -dt, so, bs, stk, binds)) //Execute (not implemented)
      case 'h' => FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, -dt), -dt, so, bs, stk, binds))
      case 'l' => FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, -dt), -dt, so, bs, stk, binds))
      case _ => FIPCont(prog, inp, FIP(id, prog.getNextInd(ip, -dt), -dt, so, bs, stk, binds))
    }
  }
  
  def chomp(inp: Seq[Char]): (String, Seq[Char]) = inp.splitAt(inp.indexOf('\n')) match{
    case (hd, tl) => (hd.mkString, tl.tail)}
  def chompNum(inp: Seq[Char]): (Int, Seq[Char]) = (inp.takeWhile(_.isDigit).mkString.toInt, inp.dropWhile(c => !c.isDigit).dropWhile(_.isDigit))
  
  def TOSS: FungeStack = stk.head
  def SOSS: FungeStack = stk(1)
  
  def setID(nid: Int): FIP = FIP(nid, ip, dt, so, bs, stk, binds)
  
  override def toString: String = s"{id=$id, ip=$ip, dt=$dt, so=$so, bs=$bs, stk = [${stk.map(lst => s"[${lst.take(4).mkString(", ")}, ...]").take(2).mkString(" ")}]}"
}

object SysInf{
  val tFlg = 1
  val iFlg = 0
  val oFlg = 0
  val eqFlg = 0
  val bufFlg = 0
  
  val bytesPerCell = 4
  val handPrint = 1165193033 //EsoI encoded as a semantic label
  val version = 1
  val paradigm = 0
  val pathSep = '/'
  val dims = 2
}