package parsers

import scala.annotation.tailrec

trait ARPRet[+A, +B]
case class ARPNext[+A, +B](res: B, rem: A, start: Int, end: Int) extends ARPRet[A, B]
case class ARPDown[+A, +B](rem: A, start: Int, end: Int) extends ARPRet[A, B]
case class ARPUp[+A, +B](rem: A, start: Int, end: Int) extends ARPRet[A, B]
object ARPFail extends ARPRet[Nothing, Nothing]

case class ArbitraryRecurParser[A, B](func: PartialFunction[A, ARPRet[A, B]], collect: Seq[B] => B) extends OrderedParser[A, B]{
  def apply(inp: A): EsoParseRes[A, B] = {
    @tailrec
    def pdo(src: A, end: Int, tmp: Vector[B], stk: Vector[Vector[B]]): ARPRet[A, B] = func.lift(src) match{
      case None => ARPFail
      case Some(nxt) => nxt match{
        case ARPDown(rem, _, ne) => pdo(rem, end + ne, Vector(), tmp +: stk)
        case ARPUp(rem, _, ne) => stk match{
          case t +: ts => pdo(rem, end + ne, t :+ collect(tmp), ts)
          case _ => ARPNext(collect(tmp), rem, -1, -1)}
        case ARPNext(res, rem, _, ne) => pdo(rem, end + ne, tmp :+ res, stk)
        case arp => arp}}
    
    def comb(start: Int, ps: ARPRet[A, B]): EsoParseRes[A, B] = ps match{
      case ARPNext(res, rem, _, end) => EsoParsed(res, rem, start, end)
      case _ => EsoParseFail}
    
    func.lift(inp) match{
      case None => EsoParseFail
      case Some(nxt) => nxt match{
        case ARPNext(res, rem, start, end) => comb(start, pdo(rem, end, Vector(res), Vector()))
        case ARPDown(rem, start, end) => comb(start, pdo(rem, end, Vector(), Vector()))
        case _ => EsoParseFail}}}
}

object ArbitraryRecurParser{
  def apply[A, B](func: A => ARPRet[A, B], collect: Seq[B] => B): ArbitraryRecurParser[A, B] = {
    ArbitraryRecurParser(PartialFunction.fromFunction(func), collect)}
}