package emmental

import common.{Config, Interpreter}

import scala.annotation.tailrec
import scala.util.Try

object Emmental extends Interpreter{
  val name: String = "Emmental"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    @tailrec
    def emi(s: EMState): Option[(Char, EMState)] = s.doOp() match{
      case Left(s2) => emi(s2)
      case Right(op) => op
    }
    
    Try{input => LazyList.unfold(EMState(progRaw, input))(emi)}
  }
}
