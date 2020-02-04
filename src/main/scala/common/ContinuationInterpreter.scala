package common

import scala.annotation.tailrec

object ContinuationInterpreter extends EsoObj{
  def apply(initState: IState): LazyList[Char] = {
    @tailrec
    def rdo(state: IState): Option[(String, IState)] = state match{
      case IHaltState => None
      case IPrintState(str, next) => Some((str, next))
      case _ => rdo(state.next)}
    
    LazyList.unfold(initState)(rdo).flatten}
}

trait IState{
  def next: IState}

object IHaltState extends IState{
  def next: IState = IHaltState}

case class IPrintState(str: String, next: IState) extends IState

trait ICont[E]{
  def apply(env: E): IState}

case class IHaltCont[E](default: E) extends ICont[E]{
  def apply(env: E): IState = IHaltState}