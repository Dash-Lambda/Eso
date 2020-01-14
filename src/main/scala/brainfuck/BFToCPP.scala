package brainfuck


object BFToCPP extends BFTranspiler{
  val dst: String = "C++"
  
  def genProg(init: Int, olen: Int, dyn: Boolean, methSize: Int, prog: Vector[(Char, Either[Int, BlkOp])]): String = {
    def copStr(bop: BlkOp): String = {
      val opstr = bop.ops map {
        case (i, Some(n)) =>
          if(i == 0) s"tape[p] ${incStr(n)};"
          else s"tape[p ${shiftStr(i)}] ${incStr(n)};"
        case (i, None) => s"tape[p ${shiftStr(i)}] = 0;"}
      s"${opstr.mkString("\n")}${if (bop.shift != 0) s"\np ${incStr(bop.shift)};" else ""}"}
  
    def clopStr(bop: BlkOp): String = {
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
      case (op, arg) => arg match{
        case Left(num) => op match{
          case 'm' => s"p ${incStr(num)};"
          case '/' => s"while(tape[p] != 0){p ${incStr(num)};}"
          case '[' => "while(tape[p] != 0){"
          case ']' => "}"
          case ',' => "tape[p] = getchar();"
          case '.' => "putchar(tape[p]);"
          case 'e' => ""}
        case Right(bop) => op match{
          case 'u' | 'a' => copStr(bop)
          case 'l' => clopStr(bop)}}}
  
    s"""|#include <stdio.h>
        |
        |int main(){
        |int tape[$init];
        |int p = 0;
        |
        |${toProg.mkString("\n")}
        |}""".stripMargin}
}
