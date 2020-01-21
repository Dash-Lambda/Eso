package snusp

import common.{Config, Matrix, Transpiler}

import scala.annotation.tailrec
import scala.util.Try

object BFToSNUSP extends Transpiler{
  val src: String = "BrainFuck"
  val dst: String = "SNUSP"
  
  
  def apply(config: Config)(progRaw: String): Try[String] = {
    def prog: LazyList[Char] = filterChars(progRaw, "[]<>+-,.").replaceAllLiterally("[]", "[ ]").to(LazyList)
    
    def redirect(progSrc: LazyList[Char]): Vector[Char] = {
      def rdo(src: LazyList[Char], ac: Vector[Char] = Vector()): (Vector[Char], LazyList[Char]) = src match{
        case '[' +: cs =>
          val (blk, tl) = rdo(cs)
          val rev = blk.reverse.map{
            case '[' => ']'
            case ']' => '['
            case c => c}
          rdo(tl, ac ++ ('[' +: rev :+ ']'))
        case ']' +: cs => (ac, cs)
        case c +: cs => rdo(cs, ac :+ c)
        case _ => (ac, src)}
      rdo(progSrc)._1}
    
    def setSafe(mat: Matrix[Char], x: Int, y: Int, e: Char): Matrix[Char] = {
      if(mat.isDefinedAt(x, y)) mat.updated(x, y, e)
      else mat.padToAbs(x + 1, y + 1, ' ').updated(x, y, e)}
    
    @tailrec
    def tdo(x: Int, y: Int, src: Vector[Char], ac: Matrix[Char]): Matrix[Char] = src match{
      case c +: cs => c match{
        case '[' =>
          val nac = {
            if(y%2 == 0)
              setSafe(ac, x + 1, y + 1, '\\')
                .updated(x, y, '!')
                .updated(x + 1, y, '/')
            else
              setSafe(ac, x + 1, y + 1, ' ')
                .updated(x, y, '/')
                .updated(x, y + 1, '\\')
                .updated(x + 1, y, '?')}
          tdo(x + 2 - (y%2), y + 1, cs, nac)
        case ']' =>
          val nac = {
            if(y%2 == 1)
              setSafe(ac, x, y, '/')
                .updated(x, y - 1, '\\')
                .updated(x - 1, y - 1, '?')
            else
              setSafe(ac, x + 1, y - 1, '!')
                .updated(x, y - 1, '\\')
                .updated(x, y, '/')}
          tdo(x + 2 - (y%2), y - 1, cs, nac)
        case _ => tdo(x + 1, y, cs, setSafe(ac, x, y, c))}
      case _ => ac}
    Try{tdo(0, 0, redirect(prog), Matrix.fill(prog.size, 1)(' ')).denseString}}
}
