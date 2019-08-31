package common

import scala.annotation.tailrec
import scala.util.Try

trait Generator {
  val src: String
  val dst: String
  
  def id: (String, String) = (src, dst)
  def apply(config: Config)(progRaw: String): Try[String]
  
  def indent(prog: String): String = {
    @tailrec
    def ido(ind: Int, ac: String, src: LazyList[String]): String = src match{
      case l +: ls =>
        val s = l.count(_ == '{') - l.count(_ == '}')
        if(l.startsWith("}")) ido(ind + s, ac ++ s"${"\t"*(ind - 1)}$l\n", ls)
        else ido(ind + s, ac ++ s"${"\t"*ind}$l\n", ls)
      case _ => ac
    }
    
    val lines = prog.split("(\r\n|\r|\n)").to(LazyList).map(_.dropWhile("\t ".contains(_)))
    ido(0, "", lines)
  }
}
