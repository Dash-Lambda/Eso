package scala_internal

import java.util.concurrent.BlockingQueue

import scala.util.{Failure, Success, Try}

object ScalaInternRun extends (String => Try[Seq[Char] => LazyList[Char]]){
  def apply(progRaw: String): Try[Seq[Char] => LazyList[Char]] = ScalaFactory(progRaw)
  
  def consumer(queue: BlockingQueue[Option[Try[Char]]]): LazyList[Char] = {
    LazyList.unfold((): Unit)(_ => queue.take map((_, ()))) map{
      case Success(c) => c
      case Failure(e) => throw e}}
}
