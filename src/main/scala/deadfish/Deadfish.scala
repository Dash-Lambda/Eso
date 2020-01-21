package deadfish

import common.{Config, Interpreter}

import scala.annotation.tailrec
import scala.util.Try

object Deadfish extends Interpreter{
  val name: String = "Deadfish"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = Try{config.bool("dfChar")} map{
    dfc =>
      if(dfc) _ => dfRun(progRaw).map(_.toChar)
      else _ => dfRun(progRaw).flatMap(_.toString)}
  
  def dfRun(progRaw: String): LazyList[Int] = {
    val prog = filterChars(progRaw, "isdo").toVector
    
    def bound(n: Int): Int = if(n == -1 || n == 256) 0 else n
    
    @tailrec
    def dfi(n: Int, src: Vector[Char]): Option[(Int, (Int, Vector[Char]))] = src match{
      case c +: cs => c match{
        case 'i' => dfi(bound(n + 1), cs)
        case 'd' => dfi(bound(n - 1), cs)
        case 's' => dfi(bound(n*n), cs)
        case 'o' => Some((n, (n, cs)))}
      case _ => None}
    LazyList.unfold((0: Int, prog)){case (n, src) => dfi(n, src)}}
}
