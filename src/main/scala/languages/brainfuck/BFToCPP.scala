package languages.brainfuck


object BFToCPP extends BFTranspiler{
  val dst: String = "C++"
  
  def genProg(init: Int, olen: Int, dyn: Boolean, methSize: Int, prog: Vector[BFOp]): String = {
    def copStr(bop: SingOp): String = {
      val opstr = bop.ops map {
        case (i, Some(n)) =>
          if(i == 0) s"tape[p] ${incStr(n)};"
          else s"tape[p ${shiftStr(i)}] ${incStr(n)};"
        case (i, None) => s"tape[p ${shiftStr(i)}] = 0;"}
      s"${opstr.mkString("\n")}${if (bop.shift != 0) s"\np ${incStr(bop.shift)};" else ""}"}
  
    def clopStr(bop: LoopOp): String = {
      val opstr = bop.ops
        .filter(_._1 != 0)
        .map {
          case (i, Some(n)) => s"tape[p ${shiftStr(i)}] ${incStr(n)}*tmp;"
          case (i, None) => s"tape[p ${shiftStr(i)}] = 0;"}
      s"""|if(tape[p] != 0){
          |int tmp = tape[p];
          |${opstr.mkString("\n")}
          |tape[p] = 0;
          |}""".stripMargin}
    
    val toProg = prog map{
      case BFMove(n) => s"p ${incStr(n)};"
      case BFScan(n) => s"while(tape[p] != 0){p ${incStr(n)};}"
      case BFOpenLoop(_) => "while(tape[p] != 0){"
      case BFCloseLoop(_) => "}"
      case BFOut(n) => s"for(int cnt = 0; cnt < $n; cnt++){putchar(tape[p]);}"
      case BFIn => "tape[p] = getchar();"
      case BFEnd => ""
      case bop: SingOp => copStr(bop)
      case bop: LoopOp => clopStr(bop)}
  
    s"""|#include <stdio.h>
        |
        |int main(){
        |int tape[$init];
        |int p = 0;
        |
        |${toProg.mkString("\n")}
        |}""".stripMargin}
}
