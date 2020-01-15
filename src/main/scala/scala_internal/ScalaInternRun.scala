package scala_internal

import java.util.concurrent.{BlockingQueue, SynchronousQueue}

import scala.util.{Failure, Success, Try}

object ScalaInternRun extends (String => Try[Seq[Char] => LazyList[Char]]){
  def apply(progRaw: String): Try[Seq[Char] => LazyList[Char]] = ScalaFactory(progRaw) map{func =>
    {inputs =>
      val synQueue = new SynchronousQueue[Option[Try[Char]]]()
      new Thread(func(synQueue, inputs)).start()
      consumer(synQueue)}}
  
  def consumer(queue: BlockingQueue[Option[Try[Char]]]): LazyList[Char] = {
    LazyList.unfold((): Unit)(_ => queue.take map((_, ()))) map{
      case Success(c) => c
      case Failure(e) => throw e}}
}
