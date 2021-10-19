package languages.brainfuck

import common.{Config, Transpiler}

import scala.util.Try

object DFToBF extends Transpiler{
  val src: String = "Deadfish"
  val dst: String = "BrainFuck"
  
  def apply(config: Config)(progRaw: String): Try[String] = Try{
    progRaw.flatMap{
      case 'i' | 'x' => "+"
      case 'd' => "-"
      case 'o' | 'c' =>
        if(config.bool("dfChar")) "."
        else ">[-]>[-]+>[-]+<[>[-<-<<[->+>+<<]>[-<+>]>>]++++++++++>[-]+>[-]>[-]>[-]<<<<<[->-[>+>>]>[[-<+>]+>+>>]<<<<<]>>-[-<<+>>]<[-]++++++++[-<++++++>]>>[-<<+>>]<<]<[.[-]<]<"
      case 's' | 'k' => "[->>+>+<<<]>>>[-<[-<+<+>>]<[->+<]>>]<[-]<<"
      case _ => ""}}
}
