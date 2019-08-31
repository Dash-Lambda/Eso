package scalarun

import java.util.concurrent.{BlockingQueue, SynchronousQueue}

import common.{Config, EsoExcep, Interpreter}

import scala.util.{Failure, Success, Try}

object ScalaRun extends Interpreter{
  val name: String = "Scala"
  
  def apply(config: Config)(progRaw: String): Try[Seq[Char] => LazyList[Char]] = ScalaFactory(progRaw) map{func =>
    {inputs =>
      val synQueue = new SynchronousQueue[Option[Try[Char]]]()
      val runner: Runnable = func(synQueue, inputs)
      new Thread(runner).start()
      consumer(synQueue)
    }
  }
  
  def consumer(queue: BlockingQueue[Option[Try[Char]]]): LazyList[Char] = {
    LazyList.unfold((): Unit)(_ => queue.take map((_, ()))) map{
      case Success(c) => c
      case Failure(e) => throw e
    }
  }
}
