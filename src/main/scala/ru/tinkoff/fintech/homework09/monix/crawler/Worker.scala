package ru.tinkoff.fintech.homework09.monix.crawler
//import akka.actor.Status.{Failure, Success}
import scala.util.{Success, Failure}
import monix.eval.{Fiber, Task}

trait Worker {
  def http: Http
  def parseLinks: Parsr

  def worker(workerQueue: MQueue[Url], crawlerQueue: MQueue[CrawlerMessage]): Task[Fiber[Unit]] =
    workerQueue.take.flatMap(url => {
      http.get(url).materialize.map {
        case Failure(_) => List.empty
        case Success(body) => parseLinks.links(body)
      }.flatMap(urls => crawlerQueue.offer(CrawlResult(url, urls)))}).restartUntil(_ => false).start
}
