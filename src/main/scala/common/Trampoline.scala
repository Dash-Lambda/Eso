package common

import org.typelevel.jawn.ast.{JNull, JValue}
import ui.{EsoConsoleInterface, EsoIOInterface}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

sealed trait Trampoline[A]
case class Done[A](res: A) extends Trampoline[A]
case class More[A](call: () => Trampoline[A]) extends Trampoline[A]

trait FlatOp[A] extends Trampoline[A]{
  def collapse(): Option[Trampoline[A]]}

case class DoOrOp[A, B](inp: Option[A], err: String, eio: EsoIOInterface = EsoConsoleInterface)(f: A => Trampoline[B]) extends FlatOp[B]{
  def collapse(): Option[Trampoline[B]] = inp match{
    case Some(a) => Some(More(() => f(a)))
    case None =>
      eio.println(s"Error: $err")
      None}}

case class DoOrErr[A, B](inp: Try[A], eio: EsoIOInterface = EsoConsoleInterface)(f: A => Trampoline[B]) extends FlatOp[B]{
  def collapse(): Option[Trampoline[B]] = inp match{
    case Success(a) => Some(More(() => f(a)))
    case Failure(e) =>
      e match{
        case EsoExcep(info) => eio.println(s"Error: common.EsoExcep ($info)")
        case _ => eio.println(s"Error: $e")}
      None}}

case class DoOrNull[B](inp: JValue, err: String, eio: EsoIOInterface = EsoConsoleInterface)(f: JValue => Trampoline[B]) extends FlatOp[B]{
  def collapse(): Option[Trampoline[B]] = inp match{
    case JNull =>
      eio.println(s"Error: $err")
      None
    case _ => Some(f(inp))}}

object Trampoline{
  def apply[A](initTramp: Trampoline[A]): Option[A] = {
    @tailrec
    def tdo(tramp: Trampoline[A]): Option[A] = tramp match{
      case Done(a) => Some(a)
      case More(call) => tdo(call())
      case fop: FlatOp[A] => fop.collapse() match{
        case Some(tmp) => tdo(tmp)
        case None => None}}
  tdo(initTramp)}
  
  def doOrElse[A](default: A)(initTramp: Trampoline[A]): A = {
    @tailrec
    def tdo(tramp: Trampoline[A]): A = tramp match{
      case Done(a) => a
      case More(call) => tdo(call())
      case fop: FlatOp[A] => fop.collapse() match{
        case Some(nxt) => tdo(nxt)
        case None => default}}
    tdo(initTramp)}
}