package languages.senpai

import common.{Config, Interpreter}
import parsers.CombinatorFuncs.all
import parsers._
import parsers.EsoParser._

import scala.collection.immutable
import scala.util.Try
import scala.util.control.TailCalls._


object Senpai extends Interpreter{
  val name: String = "Senpai"
  
  trait Expression{
    def evalRec(env: SenpaiContext): TailRec[LiteralExpression]
    def evaluate(env: SenpaiContext): LiteralExpression = evalRec(env).result}
  
  trait LiteralExpression extends Expression{
    def truthiness: Boolean
    def isIntegral: Boolean
    def evalRec(env: SenpaiContext): TailRec[LiteralExpression] = done(this)}
  
  case class ErrorExpression(err: String) extends LiteralExpression{
    def truthiness: Boolean = false
    def isIntegral: Boolean = false
    override def toString: String = s"Evaluation Error: $err"}
  
  case class FunctionExpression(args: Vector[String], ops: Vector[Operation]) extends LiteralExpression{
    def truthiness: Boolean = true
    def isIntegral: Boolean = false}
  
  case class StringExpr(str: String) extends LiteralExpression{
    def truthiness: Boolean = str.nonEmpty
    def isIntegral: Boolean = false
    override def toString: String = str}
  
  case class NumericExpr(x: BigDecimal) extends LiteralExpression{
    def truthiness: Boolean = x != 0
    def isIntegral: Boolean = (x%1) == 0
    override def toString: String = x.toString}
  
  case class BoolExpr(b: Boolean) extends LiteralExpression{
    def truthiness: Boolean = b
    def isIntegral: Boolean = false
    override def toString: String = if(b) "True" else "False"}
  
  case class ToCharExpr(exp: Expression) extends Expression{
    def evalRec(env: SenpaiContext): TailRec[LiteralExpression] = exp.evalRec(env).map{
      case NumericExpr(n) => StringExpr(n.toChar.toString)}}
  
  case class NameExpr(name: String) extends Expression{
    def evalRec(env: SenpaiContext): TailRec[LiteralExpression] = tailcall(env.retrieve(name).get.evalRec(env))}
  
  case class UnaryOpExpr(op: String, xe: Expression) extends Expression{
    def evalRec(env: SenpaiContext): TailRec[LiteralExpression] = {
      xe.evalRec(env) map (x =>
        op match{
          case "flipped" => BoolExpr(x.truthiness)
          case "negative" => x match{
            case NumericExpr(a) => NumericExpr(-a)}})}}
  
  case class BinOpExpr(op: String, xe: Expression, ye: Expression) extends Expression{
    def evalRec(env: SenpaiContext): TailRec[LiteralExpression] = {
      xe.evalRec(env).flatMap(x =>
        ye.evalRec(env).map(y =>
          op match{
            case "and" => (x, y) match{
              case (NumericExpr(a), NumericExpr(b)) => NumericExpr(a + b)
              case (StringExpr(a), StringExpr(b)) => StringExpr(a ++ b)}
            case "minus" => (x, y) match{
              case (NumericExpr(a), NumericExpr(b)) => NumericExpr(a - b)}
            case "times" => (x, y) match{
              case (NumericExpr(a), NumericExpr(b)) => NumericExpr(a*b)
              case (NumericExpr(n), StringExpr(s)) => StringExpr(s*n.toInt)
              case (StringExpr(s), NumericExpr(n)) => StringExpr(s*n.toInt)}
            case "divided by" => (x, y) match{
              case (NumericExpr(a), NumericExpr(b)) => NumericExpr(a/b)}
            case "or" if x.isIntegral && y.isIntegral => (x, y) match{
              case (NumericExpr(a), NumericExpr(b)) => NumericExpr(a.toInt | b.toInt)}
            case "combined" if x.isIntegral && y.isIntegral => (x, y) match{
              case (NumericExpr(a), NumericExpr(b)) => NumericExpr(a.toInt & b.toInt)}
            case "exclusively or" if x.isIntegral && y.isIntegral => (x, y) match{
              case (NumericExpr(a), NumericExpr(b)) => NumericExpr(a.toInt ^ b.toInt)}
            case "mod" if x.isIntegral && y.isIntegral => (x, y) match{
              case (NumericExpr(a), NumericExpr(b)) => NumericExpr(a % b)}
            case "is equal to" => BoolExpr(x == y)
            case "is not equal to" => BoolExpr(x != y)
            case "is smaller than" => (x, y) match{
              case (NumericExpr(a), NumericExpr(b)) => BoolExpr(a < b)}
            case "is less than or equal to" => (x, y) match{
              case (NumericExpr(a), NumericExpr(b)) => BoolExpr(a <= b)}
            case "is greater than or equal to" => (x, y) match{
              case (NumericExpr(a), NumericExpr(b)) => BoolExpr(a >= b)}
            case "either or" => BoolExpr(x.truthiness || y.truthiness)
            case "and also" => BoolExpr(x.truthiness && y.truthiness)}))}}
  
  trait Operation
  case class Declare(name: String) extends Operation
  case class Assign(name: String, expr: Expression) extends Operation
  case class Delete(name: String) extends Operation
  
  case class Push(name: String) extends Operation
  object Pop extends Operation
  object Swap extends Operation
  object Rotate extends Operation
  case class SwitchStack(name: String) extends Operation
  case class AssignFromTop(name: String) extends Operation
  
  case class IfCond(cond: Expression, bias: Boolean, a: Vector[Operation], b: Vector[Operation]) extends Operation
  case class WhileCond(cond: Expression, bias: Boolean, ops: Vector[Operation]) extends Operation
  
  case class FunctionBlock(name: String, args: Vector[String], ops: Vector[Operation]) extends Operation
  case class CallFunction(args: Int) extends Operation
  
  case class ImportModule(path: String) extends Operation
  
  trait SenpaiContext{
    def declare(name: String): SenpaiContext
    def assign(name: String, expr: Expression): Option[SenpaiContext]
    def retrieve(name: String): Option[Expression]
    def delete(name: String): SenpaiContext
    def stepOut: Option[SenpaiContext]
    def stepIn(newProg: Vector[Operation]): SenpaiContext = TopContext(immutable.HashMap(), newProg, this)
    def op: Option[Operation]
    def next: SenpaiContext
    def pushOp(op: Operation): SenpaiContext
    def vars: immutable.HashMap[String, Expression]
    def allVars: Vector[(String, Expression)]
    def varString: String}
  
  object BottomContext extends SenpaiContext{
    def declare(name: String): SenpaiContext = BottomContext
    def assign(name: String, expr: Expression): Option[SenpaiContext] = None
    def retrieve(name: String): Option[Expression] = None
    def delete(name: String): SenpaiContext = this
    def stepOut: Option[SenpaiContext] = None
    def op: Option[Operation] = None
    def next: SenpaiContext = this
    def pushOp(op: Operation): SenpaiContext = TopContext(immutable.HashMap(), Vector(op), this)
    def vars: immutable.HashMap[String, Expression] = immutable.HashMap()
    def allVars: Vector[(String, Expression)] = Vector()
    def varString: String = ""}
  
  case class TopContext(vars: immutable.HashMap[String, Expression], prog: Vector[Operation], parent: SenpaiContext) extends SenpaiContext{
    def declare(name: String): SenpaiContext = TopContext(vars.updated(name, BoolExpr(false)), prog, parent)
    def assign(name: String, expr: Expression): Option[SenpaiContext] =
      if(vars.exists(_._1 == name)) Some(TopContext(vars.updated(name, expr.evaluate(this)), prog, parent))
      else parent.assign(name, expr) map (p => TopContext(vars, prog, p))
    def retrieve(name: String): Option[Expression] = vars.get(name) orElse parent.retrieve(name)
    def delete(name: String): SenpaiContext = TopContext(vars.removed(name), prog, parent)
    def stepOut: Option[SenpaiContext] = Some(parent)
    def op: Option[Operation] = prog.headOption
    def next: SenpaiContext = prog match{
      case _ +: ops => TopContext(vars, ops, parent)
      case _ => parent}
    def pushOp(op: Operation): SenpaiContext = TopContext(vars, op +: prog, parent)
    def pushOps(ops: Vector[Operation]): SenpaiContext = TopContext(vars, ops ++: prog, parent)
    def allVars: Vector[(String, Expression)] = vars.toVector ++ parent.allVars
    def varString: String = allVars.map{case (k, v) => s"($k=$v)"}.mkString("{", ", ", "}")}
  
  trait SenpaiState{
    def next: SenpaiState
    def run: TailRec[Option[(String, SenpaiState)]]}
  
  case class SenpaiSpeak(str: String, next: SenpaiState) extends SenpaiState{
    def run: TailRec[Option[(String, SenpaiState)]] = done(Some((str, next)))}
  
  case class SenpaiStop(code: Int) extends SenpaiState{
    def run: TailRec[Option[(String, SenpaiState)]] = done(None)
    def next: SenpaiState = this}
  
  case class SenpaiRun(curStack: Vector[Expression], curName: String, stacks: Vector[(String, Vector[Expression])], env: SenpaiContext, inp: Seq[Char]) extends SenpaiState{
    def stackOp(ns: Vector[Expression]): SenpaiRun = SenpaiRun(ns, curName, stacks, env, inp)
    def nameOp(nn: String): SenpaiRun = SenpaiRun(curStack, nn, stacks, env, inp)
    def envOp(ne: SenpaiContext): SenpaiRun = SenpaiRun(curStack, curName, stacks, ne, inp)
    def inpOp(ni: Seq[Char]): SenpaiRun = SenpaiRun(curStack, curName, stacks, env, ni)
    
    def nextOp: SenpaiRun = envOp(env.next)
    def changeStacks(ns: Vector[(String, Vector[Expression])]): SenpaiRun = SenpaiRun(curStack, curName, ns, env, inp)
    def readLine(src: Seq[Char] = inp, ac: String = ""): (String, Seq[Char]) = src match{
      case '\n' +: rem => (ac, rem)
      case c +: rem => readLine(rem, ac + c)}
    
    def run: TailRec[Option[(String, SenpaiState)]] = tailcall(next.run)
    def next: SenpaiState = env.op match{
        case None => env.stepOut match{
          case Some(nxt) => SenpaiRun(curStack, curName, stacks, nxt, inp)
          case None => SenpaiStop(0)}
        case Some(op) => op match{
          case Declare(name) => envOp(env.declare(name)).nextOp
          case Assign(name, expr) => envOp(env.assign(name, expr).get).nextOp
          case Delete(name) => envOp(env.delete(name)).nextOp
          
          case Push(name) => stackOp(NameExpr(name) +: curStack).nextOp
          case Pop => stackOp(curStack.tail).nextOp
          case Swap => stackOp(curStack match{case a +: b +: as => b +: a +: as}).nextOp
          case Rotate => stackOp(curStack match{case a +: b +: c +: as => b +: c +: a +: as}).nextOp
          case SwitchStack(name) =>
            if(name == curName) nextOp
            else stacks.collectFirst{case (`name`, vec) => vec} match{
              case Some(v) => stackOp(v).nameOp(name).changeStacks((curName, curStack) +: stacks.filter(_._1 != name))
              case None => stackOp(Vector()).nameOp(name).changeStacks((curName, curStack) +: stacks)}
          case AssignFromTop(name) => envOp(env.assign(name, curStack.head).get).stackOp(curStack.tail).nextOp
          case IfCond(cond, bias, a, b) => if(cond.evaluate(env).truthiness == bias) envOp(env.next.stepIn(a)) else envOp(env.next.stepIn(b))
          case WhileCond(cond, bias, a) => if(cond.evaluate(env).truthiness == bias) envOp(env.stepIn(a)) else nextOp
          
          case FunctionBlock(name, args, a) => envOp(env.assign(name, FunctionExpression(args, a)).get).nextOp
          case CallFunction(args) => curStack.head match{
            case NameExpr("love") => SenpaiSpeak(curStack.tail.take(args).map(_.evaluate(env)).mkString("", " ", "\n"), stackOp(curStack.drop(args + 1)).nextOp)
            case NameExpr("reason") if args == 0 => readLine() match{case (tok, rem) => stackOp(StringExpr(tok) +: curStack.tail).inpOp(rem).nextOp}
            case NameExpr("reason") if args == 1 => SenpaiSpeak(curStack.tail.head.toString, stackOp(NameExpr("reason") +: curStack.tail.tail).nextOp.envOp(env.pushOp(CallFunction(0))))
            case NameExpr("crash") if args == 0 => SenpaiStop(-1)
            case NameExpr("crash") if args == 1 => curStack.tail.head.evaluate(env) match{
              case NumericExpr(n) => SenpaiStop(n.toInt)
              case _ => SenpaiStop(-1)}
            case NameExpr(name) => env.retrieve(name) match{
              case Some(FunctionExpression(args, ops)) =>
                val nops = args.zip(curStack).map{case (n, e) => Assign(n, e)}
                stackOp(curStack.drop(args.length + 1)).envOp(env.next.stepIn(nops ++: ops))}}
          case ImportModule(path) => ???}}}
  
  def blankParse: EsoParser[String] = R("""^\s*""".r)
  
  //Literal Datatypes
  def nameParse: EsoParser[String] = R("""^[a-zA-Z_]\w*""".r)
  def integerParse: EsoParser[LiteralExpression] = R("""^-?\d+""".r) map (s => NumericExpr(BigDecimal(s)))
  def decimalParse: EsoParser[LiteralExpression] = R("""^-?\d+(?:\.\d+)?""".r) map (s => NumericExpr(BigDecimal(s)))
  def stringToken(lim: Char): EsoParser[Char] = ((C('\\') | empty("")) <&> R("""^.""".r)) collect {
    case ("\\", "r") => '\r'
    case ("\\", "n") => '\n'
    case ("\\", "t") => '\t'
    case ("\\", "b") => '\b'
    case ("\\", "f") => '\f'
    case ("\\", "\\") => '\\'
    case ("\\", s) if s.head == lim => lim
    case ("", s) if s.head != lim => s.head}
  def chompString(lim: Char): EsoParser[String] = C(lim) &> (all(stringToken(lim)) map (_.mkString)) <& C(lim)
  def stringParse: EsoParser[LiteralExpression] = (chompString('"') | chompString('\'')) map (s => StringExpr(s))
  def boolParse: EsoParser[LiteralExpression] = (S("True") ^^^ BoolExpr(true)) | (S("False") ^^^ BoolExpr(false))
  
  //Operators
  def unaryOperator: EsoParser[String] = R("""^(negative|flipped)""".r)
  def addsubOperator: EsoParser[String] = R("""^(and|minus|or|combined|exclusively or)""".r)
  def muldivOperator: EsoParser[String] = R("""^(times|divided by|mod)""".r)
  def logicOperator: EsoParser[String] = R("""^(is equal to|is not equal to|is smaller than|is less than or equal to|is greater than or equal to|either or|and also)""".r)
  
  def infixLayerParse(nextLayer: => EsoParser[Expression], operator: => EsoParser[String]): EsoParser[Expression] = (nextLayer <&> all((C(' ') &> operator <& C(' ')) <&> nextLayer)) map {case (base: Expression, vec) => vec.foldLeft(base){case (x, (op, y)) => BinOpExpr(op, x, y)}}
  def logicParse: EsoParser[Expression] = infixLayerParse(addsubParse, logicOperator)
  def addsubParse: EsoParser[Expression] = infixLayerParse(muldivParse, addsubOperator)
  def muldivParse: EsoParser[Expression] = infixLayerParse(termparenParse, muldivOperator)
  def tocharParse: EsoParser[Expression] = (C('[') &> addsubParse <& C(']')) map ToCharExpr
  def refParse: EsoParser[NameExpr] = nameParse map NameExpr
  def literalParse: EsoParser[LiteralExpression] = stringParse | integerParse | decimalParse | boolParse
  def termparenParse: EsoParser[Expression] = literalParse | refParse | tocharParse | tocharParse | (C('(') &> logicParse <& C(')')) | ((unaryOperator <&> addsubParse) map {case (op, exp) => UnaryOpExpr(op, exp)})
  def exprParse: EsoParser[Expression] = logicParse
  
  //Variable Operations
  def declareParse: EsoParser[Operation] = S("Senpai? Can I see your ") &> (nameParse map Declare) <& S("?")
  def assignParse: EsoParser[Operation] = (S("Your ") &> nameParse <&> (S(" is very ") &> exprParse) <& S("!")) map {case (nam, exp) => Assign(nam, exp)}
  def deleteParse: EsoParser[Operation] = S("Get rid of") &> (nameParse map Delete) <& S("!")
  
  def variableParse: EsoParser[Operation] = declareParse | assignParse | deleteParse
  
  //Stack Operations
  def pushParse: EsoParser[Operation] = S("Show me your ") &> (nameParse map Push) <& S("!")
  def popParse: EsoParser[Operation] = S("I don't like it anymore!") ^^^ Pop
  def swapParse: EsoParser[Operation] = S("Let's switch things up a bit!") ^^^ Swap
  def rotateParse: EsoParser[Operation] = S("Let's really switch things up!") ^^^ Rotate
  def switchStackParse: EsoParser[Operation] = S("Let's take it to the ") &> (nameParse map SwitchStack) <& S("!")
  def assignFromTopParse: EsoParser[Operation] = S("Let's bring this to ") &> (nameParse map AssignFromTop) <& S("!")
  
  def stackOpParse: EsoParser[Operation] = pushParse | popParse | swapParse | rotateParse | switchStackParse | assignFromTopParse
  
  //Flow Control
  def biasParse: EsoParser[Boolean] = (S("likey") ^^^ true) | (S("no-likey") ^^^ false)
  def elseParse: EsoParser[Vector[Operation]] = blankParse &> ((S("Otherwise:") &> senpaiParse) | empty(Vector())) <& blankParse <& S("Let's move on now!")
  def ifParse: EsoParser[Operation] = S("If you ") &> ((biasParse <& S(" ")) <&> ((exprParse <& S(":")) <&> (senpaiParse <&> elseParse))) map {case (bias, (cond, (a, b))) => IfCond(cond, bias, a, b)}
  def whileParse: EsoParser[Operation] = S("Let's keep this going as long as you ") &> ((biasParse <& S(" ")) <&> ((exprParse <& S(":")) <&> senpaiParse) <& blankParse <& S("We can stop now!")) map {case (bias, (cond, ops)) => WhileCond(cond, bias, ops)}
  
  def controlFlowParse: EsoParser[Operation] = ifParse | whileParse
  
  //Functions
  def argListParse: EsoParser[Vector[String]] = ((all(nameParse <& S(", "), 2) <&> (S("and ") &> nameParse)) map {case (v, x) => v :+ x}) | ((nameParse <&> (S(" and ") &> nameParse)) map {case (a, b) => Vector(a, b)}) | (nameParse map (a => Vector(a)))
  def argsParse: EsoParser[Vector[String]] = (S(" It needs ") &> argListParse <& S("to do it!")) | (S("") ^^^ Vector())
  def funcParse: EsoParser[Operation] = (((nameParse <&> (S(" is my idea!") &> argsParse)) <& blankParse <& S("Here it is:")) <&> (senpaiParse <& blankParse <& S("That's it!"))) map {case ((nam, args), ops) => FunctionBlock(nam, args, ops)}
  def callParse: EsoParser[Operation] = R("""^Notice me, senpai(!*)""".r) map (s => CallFunction(s.length))
  
  def functionParse: EsoParser[Operation] = funcParse | callParse
  
  //Modules
  def moduleParse: EsoParser[Operation] = S("""Senpai? Do you see """") &> (stringParse map {case StringExpr(s) => ImportModule(s)}) <& S(""""?""")
  
  //Oh yeah, it's all coming together...
  def instructionParse: EsoParser[Operation] = (variableParse | stackOpParse | controlFlowParse | functionParse | moduleParse) mapAll {case (op, _, _, _) => op}
  def lineParse: EsoParser[Operation] = blankParse &> instructionParse
  def senpaiParse: EsoParser[Vector[Operation]] = all(lineParse, 1)
  
  def runSenpai(prog: Vector[Operation])(inp: Seq[Char]): LazyList[Char] = LazyList.unfold(SenpaiRun(Vector(), "bedroom", Vector(), TopContext(immutable.HashMap(), prog, BottomContext), inp): SenpaiState)(state => state.run.result).flatten
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = {
    senpaiParse(progRaw).toTry() map{
      case (prog, _, _, _) => runSenpai(prog)}}
}
