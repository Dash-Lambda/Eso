package brainfuck

import common.{Config, Transpiler}

import scala.util.Try

trait BFTranspiler extends Transpiler{
  val src: String = "BrainFuck"
  
  def apply(config: Config)(progRaw: String): Try[String] = Try{(config.num("init"), config.num("olen"), config.num("methSize"), config.bool("dyn"), config.bool("indent"))} flatMap{
    case (init, olen, methSize, dyn, ind) =>
      BFOptimize.compOpt(progRaw) map{prog =>
        val gen = genProg(init, olen, dyn, methSize, prog)
        if(ind) indent(gen)
        else gen
      }
  }
  
  def genProg(init: Int, olen: Int, dyn: Boolean, methSize: Int, prog: LazyList[(Char, Either[Int, BlkOp])]): String
}
