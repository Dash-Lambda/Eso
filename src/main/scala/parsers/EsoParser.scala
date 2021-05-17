package parsers

import common.EsoObj

import scala.util.matching.Regex

import ParserCalls._

trait EsoParser[+A] extends ((String, Int) => ParseTramp[(A, String, Int, Int)]) with (String => ParseRes[(A, String, Int, Int)]) with EsoObj{
  override def toString: String = s"EsoParser[A]"
  import CombinatorFuncs._
  def apply(inp: String): ParseRes[(A, String, Int, Int)] = apply(inp, 0).result
  def delay(inp: String, ind: Int): ParseTramp[(A, String, Int, Int)] = ParserCalls.delayParse(this, inp, ind)
  
  def matches(inp: String): Boolean = apply(inp).passed
  
  def iterator(inp: String): Iterator[(A, String, Int, Int)] = Iterator.unfold(0: Int)(i => apply(inp, i).result.get.map(r => (r, r._4)))
  def alternatives: LazyList[EsoParser[A]] = LazyList(this)
  
  def <|[B](q: => EsoParser[B]): EsoParser[A] = lcond(this, q)
  def |>[B](q: => EsoParser[B]): EsoParser[B] = rcond(this, q)
  def &>[B](q: => EsoParser[B]): EsoParser[B] = rimp(this, q)
  def <&[B](q: => EsoParser[B]): EsoParser[A] = limp(this, q)
  def <&>[B](q: => EsoParser[B]): EsoParser[(A, B)] = prod(this, q)
  
  def |[B >: A](q: => EsoParser[B]): EsoParser[B] = alt(this, q)
  def ||[B >: A](q: => EsoParser[B]): EsoParser[B] = earliest(this, q)
  def |||[B >: A](q: => EsoParser[B]): EsoParser[B] = longest(this, q)
  
  def * : EsoParser[Vector[A]] = all(this)
  def + : EsoParser[Vector[A]] = all(this, 1)
  
  def onlyIf(cond: (A, String, Int, Int) => Boolean): EsoParser[A] = conditional(this)(cond)
  
  def ^^[U](f: A => U): EsoParser[U] = map(f)
  def ^^^[U](v: => U): EsoParser[U] = map(_ => v)
  def map[U](f: A => U): EsoParser[U] = mapped(this)(f)
  def flatMap[U](f: A => EsoParser[U]): EsoParser[U] = (inp, ind) => delay(inp, ind) flatMap{
    case (r, i, s, e) =>
      f(r).delay(i, e) map{
        case (fr, fi, _, fe) =>
          (fr, fi, s, fe)}}
  def flatMapAll[U](f: (A, String, Int, Int) => EsoParser[U]): EsoParser[U] = (inp, ind) => delay(inp, ind) flatMap{
    case (r, i, s, e) =>
      f(r, i, s, e).delay(i, e)}
  
  def LRFix[U](prev: => EsoParser[U]): EsoParser[A] = this
  def isNonLR[U](prev: => EsoParser[U]): Boolean = this != prev
  def isLR[U](prev: => EsoParser[U]): Boolean = this == prev
}
object EsoParser{
  import CombinatorFuncs.{RegexParser, StringParser, alt}
  def empty[A](v: => A): EsoParser[A] = (inp, ind) => Parsed((v, inp, ind, ind))
  def S(str: String): EsoParser[String] = StringParser(str)
  def R(regex: Regex): EsoParser[String] = RegexParser(regex)
  def R(regex: String): EsoParser[String] = RegexParser(regex.r)
  @annotation.tailrec
  def collapse[A](src: Seq[EsoParser[A]]): EsoParser[A] = src match{
    case a +: b +: ps => collapse(alt(a, b) +: ps)
    case a +: _ => a}
}

object CombinatorFuncs{
  // Primitives (combinators which other combinators are made from)
  case class RegexParser(reg: Regex) extends EsoParser[String]{
    override def toString: String = s"R($reg)"
    def apply(inp: String, ind: Int): ParseTramp[(String, String, Int, Int)] = {
      val matcher = reg.pattern.matcher(inp)
      matcher.region(ind, inp.length)
      if(matcher.find)
        if(matcher.groupCount > 0) Parsed(((1 to matcher.groupCount).map(matcher.group).mkString, inp, matcher.start, matcher.end))
        else Parsed(matcher.group(), inp, matcher.start, matcher.end)
      else ParseFail}}
  
  case class StringParser(str: String) extends EsoParser[String]{
    override def toString: String = s"S($str)"
    def apply(inp: String, ind: Int): ParseTramp[(String, String, Int, Int)] = {
      if(inp.startsWith(str, ind)) Parsed((str, inp, ind, ind + str.length))
      else ParseFail}}
  
  case class AltParser[A](parser1: () => EsoParser[A], parser2: () => EsoParser[A]) extends EsoParser[A]{
    lazy val p: EsoParser[A] = parser1()
    lazy val q: EsoParser[A] = parser2()
    
    override def alternatives: LazyList[EsoParser[A]] = LazyList.unfold(Some(this): Option[EsoParser[A]])(_.map{
      case AltParser(a, b) => (a(), Some(b()))
      case a => (a, None)})
    
    override def isNonLR[U](prev: => EsoParser[U]): Boolean = this != prev && p.isNonLR(prev) && q.isNonLR(prev)
    override def isLR[U](prev: => EsoParser[U]): Boolean = this == prev || p.isLR(prev) || q.isLR(prev)
    def collectNonLR[U](prev: => EsoParser[U]): LazyList[EsoParser[A]] = alternatives.filter(_.isNonLR(prev))
    def collectLR[U](prev: => EsoParser[U]): LazyList[EsoParser[A]] = alternatives.filter(_.isLR(prev))
    
    override def LRFix[U](prev: => EsoParser[U]): EsoParser[A] = {
      // This...
      // Is...
      // Not my most ELEGANT code.
      type FUNC = ((A, String, Int, Int), (A, String, Int, Int)) => (A, String, Int, Int)
      type PA = () => EsoParser[A]
      lazy val checkAgainst = prev
      lazy val nlrs = collectNonLR(prev)
      lazy val lrs = collectLR(prev)
      
      if(lrs.isEmpty) this
      else{
        def makeNLR(lr: EsoParser[A])(src: EsoParser[A], ins: EsoParser[A]): EsoParser[A] = src match{
          case AllFlatMapParser(p1: PA, f1) => AllFlatMapParser(() => makeNLR(lr)(p1(), ins), f1)
          case CombineParser(p1: PA, _: PA, f1: FUNC) => p1() match{
            case `checkAgainst` => CombineParser(() => ins, () => lr, f1)
            case nxt => makeNLR(lr)(nxt, ins)}}
        def makeLR(ins: => EsoParser[A])(prs: CombineParser[A, A, A]): CombineParser[A, A, A] = {
          getLRFunc(prs) match{
            case (p1, f0) => CombineParser(p1, () => ins, f0)}}
        def getLRFunc(prs: CombineParser[A, A, A]): (() => EsoParser[A], FUNC) = prs match{
          case CombineParser(p1: PA, q1: PA, f1: FUNC) => p1() match{
            case `checkAgainst` => (q1, f1)
            case nxt: CombineParser[A, A, A] => getLRFunc(nxt) match{
              case (p2, f2) => (() => CombineParser(p2, q1, f1), f2)}}}
        def makeTerm(prs: EsoParser[A]): EsoParser[A] = prs match{
          case CombineParser(p1: PA, q1: PA, f1: FUNC) => p1() match{
            case `checkAgainst` => q1()
            case nxt => CombineParser(() => makeTerm(nxt), q1, f1)}
          case _ => prs}
  
        lazy val ms: LazyList[EsoParser[A]] = lrs collect {case cp: CombineParser[A, A, A] => makeLR(m)(cp)}
        lazy val terms = lrs map makeTerm
        lazy val m: EsoParser[A] = EsoParser.collapse(ms ++ terms)
        lazy val n: EsoParser[A] = EsoParser.collapse((for(lr <- lrs; nlr <- nlrs) yield (lr, nlr)).collect{case (a, b) => makeNLR(m)(a, b)})
        AltParser(() => n, () => EsoParser.collapse(nlrs))}}
    
    def apply(inp: String, ind: Int): ParseTramp[(A, String, Int, Int)] = p.delay(inp, ind) orElse q.delay(inp, ind)}
  
  case class CombineParser[A, B, C](parser1: () => EsoParser[A], parser2: () => EsoParser[B], comp: ((A, String, Int, Int), (B, String, Int, Int)) => (C, String, Int, Int)) extends EsoParser[C]{
    lazy val p: EsoParser[A] = parser1()
    lazy val q: EsoParser[B] = parser2()
    
    override def isNonLR[U](prev: => EsoParser[U]): Boolean = p.isNonLR(prev)
    override def isLR[U](prev: => EsoParser[U]): Boolean = (p == prev) || p.isLR(prev)
  
    override def LRFix[U](prev: => EsoParser[U]): EsoParser[C] = CombineParser(() => p.LRFix(prev), () => q.LRFix(prev), comp)
    
    def apply(inp: String, ind: Int): ParseTramp[(C, String, Int, Int)] = {
      p.delay(inp, ind) flatMap{
        case (pr, pi, ps, pe) =>
          q.delay(pi, pe) map{
            case (qr, qi, qs, qe) => comp((pr, pi, ps, pe), (qr, qi, qs, qe))}}}}
  
  case class SortedAltParser[A](parser1: () => EsoParser[A], parser2: () => EsoParser[A], comp: ((A, String, Int, Int), (A, String, Int, Int)) => Boolean) extends EsoParser[A]{
    private lazy val p = parser1()
    private lazy val q = parser2()
    def apply(inp: String, ind: Int): ParseTramp[(A, String, Int, Int)] = {
      p.delay(inp, ind) flatMap{
        case (pr, pi, ps, pe) =>
          q.delay(inp, ind) flatMap{
            case (qr, qi, qs, qe) =>
              val pres = Parsed((pr, pi, ps, pe))
              val qres = Parsed((qr, qi, qs, qe))
              if(comp(pres.res, qres.res)) pres orElse qres
              else qres orElse pres} orElse Parsed((pr, pi, ps, pe))} orElse q.delay(inp, ind)}}
  
  case class IntoParser[A](parser1: () => EsoParser[String], parser2: () => EsoParser[A]) extends EsoParser[A]{
    private lazy val p = parser1()
    private lazy val q = parser2()
    def apply(inp: String, ind: Int): ParseTramp[(A, String, Int, Int)] = {
      p.delay(inp, ind) flatMap {
        case (pr, pi, ps, pe) =>
          q.delay(pr, 0) map {
            case (qr, _, _, _) =>
              (qr, pi, ps, pe)}}}}
  
  case class AllFlatMapParser[A, B](parser1: () => EsoParser[A], comp: (A, String, Int, Int) => ParseTramp[(B, String, Int, Int)]) extends EsoParser[B]{
    private lazy val p = parser1()
    def apply(inp: String, ind: Int): ParseTramp[(B, String, Int, Int)] = {
      p.delay(inp, ind) flatMap{
        case (pr, pi, ps, pe) =>
          comp(pr, pi, ps, pe)}}}
  
  @annotation.tailrec
  def alt[A](p: => EsoParser[A], q: => EsoParser[A]): EsoParser[A] = p match{
    case AltParser(a, b) => alt[A](a(), AltParser(b, () => q))
    case _ => AltParser(() => p, () => q)}
  def sortAlt[A](p: => EsoParser[A], q: => EsoParser[A])(comp: ((A, String, Int, Int), (A, String, Int, Int)) => Boolean): EsoParser[A] = SortedAltParser(() => p, () => q, comp)
  def into[A](p: => EsoParser[String], q: => EsoParser[A]): EsoParser[A] = IntoParser(() => p, () => q)
  def conditional[A](p: => EsoParser[A])(cond: (A, String, Int, Int) => Boolean): EsoParser[A] = {
    def fun(r: A, i: String, s: Int, e: Int): ParseTramp[(A, String, Int, Int)] = if(cond(r, i, s, e)) Parsed((r, i, s, e)) else ParseFail
    AllFlatMapParser(() => p, fun)}
  def allFlatMapped[A, B](p: => EsoParser[A])(comp: (A, String, Int, Int) => ParseTramp[(B, String, Int, Int)]): EsoParser[B] = AllFlatMapParser(() => p, comp)
  def mapped[A, B](p: => EsoParser[A])(f: A => B): EsoParser[B] = {
    def fun(r: A, i: String, s: Int, e: Int): ParseTramp[(B, String, Int, Int)] = Parsed((f(r), i, s, e)) // Maybe Scala 3 will have more robust lambda typing...
    AllFlatMapParser(() => p, fun)}
  def combine[A, B, C](p: => EsoParser[A], q: => EsoParser[B])(f: ((A, String, Int, Int), (B, String, Int, Int)) => (C, String, Int, Int)): CombineParser[A, B, C] = CombineParser(() => p, () => q, f)
  
  // Derived combinators
  import scala.collection.{IterableOps, IndexedSeqOps}
  def append[A, SS[_] <: IndexedSeq[_], S, B >: A](p: => EsoParser[IndexedSeqOps[A, SS, S]], q: => EsoParser[B]): EsoParser[SS[B]] = combine(p, q){case ((a, _, as, _), (b, bi, _, be)) => (a :+ b, bi, as, be)}
  def prepend[A, SS[_] <: IndexedSeq[_], S, B >: A](p: => EsoParser[B], q: => EsoParser[IndexedSeqOps[A, SS, S]]): EsoParser[SS[B]] = combine(p, q){case ((a, _, as, _), (b, bi, _, be)) => (a +: b, bi, as, be)}
  def concat[A, SS[_] <: Iterable[_], S, B >: A, T <: IterableOnce[B]](p: => EsoParser[IterableOps[A, SS, S]], q: => EsoParser[T]): EsoParser[SS[B]] = combine(p, q){case ((a, _, as, _), (b, bi, _, be)) => (a ++ b, bi, as, be)}
  def concatString(p: => EsoParser[String], q: => EsoParser[String]): EsoParser[String] = combine(p, q){case ((a, _, as, _), (b, bi, _, be)) => (a + b, bi, as, be)}
  def prod[A, B](p: => EsoParser[A], q: => EsoParser[B]): EsoParser[(A, B)] = combine(p, q){case ((a, _, as, _), (b, bi, _, be)) => ((a, b), bi, as, be)}
  
  def longest[A](p: => EsoParser[A], q: => EsoParser[A]): EsoParser[A] = sortAlt(p, q){case ((_, _, ps, pe), (_, _, qs, qe)) => (pe - ps) >= (qe - qs)}
  def earliest[A](p: => EsoParser[A], q: => EsoParser[A]): EsoParser[A] = sortAlt(p, q){case ((_, _, ps, _), (_, _, qs, _)) => ps <= qs}
  
  def all[A](parser: => EsoParser[A], num: Int = 0): EsoParser[Vector[A]] = {
    lazy val p = parser
    lazy val base = EsoParser.empty(Vector[A]())
    lazy val init: EsoParser[Vector[A]] = Vector.fill(num)(p).foldLeft(base){case (v, e) => append(v, e)}
    lazy val s: EsoParser[Vector[A]] = alt(prepend(p, s), base)
    concat(init, s)}
  
  def limp[A, B](p: => EsoParser[A], q: => EsoParser[B]): EsoParser[A] = combine(p, q){case ((a, _, as, _), (_, bi, _, be)) => (a, bi, as, be)}
  def rimp[A, B](p: => EsoParser[A], q: => EsoParser[B]): EsoParser[B] = combine(p, q){case ((_, _, as, _), (b, bi, _, be)) => (b, bi, as, be)}
  def lcond[A, B](p: => EsoParser[A], q: => EsoParser[B]): EsoParser[A] = combine(p, q){case ((a, ai, as, ae), _) => (a, ai, as, ae)}
  def rcond[A, B](p: => EsoParser[A], q: => EsoParser[B]): EsoParser[B] = combine(p, q){case (_, (b, bi, bs, be)) => (b, bi, bs, be)}
  
  def const[A, B](p: => EsoParser[A], v: => B): EsoParser[B] = p map (_ => v)
  
}