package ru.tinkoff.fintech.homework09.actor.crawler
import akka.actor.{Actor, ActorLogging, ActorRef}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

class Worker(http: Http, parser: Parsr, master: ActorRef) extends Actor with ActorLogging {
  override def receive: Receive = {
    case Crawl(url) =>
      http.get(url).onComplete {
        case Success(body) => master ! CrawlResult(url, parser.links(body))
        case Failure(_) =>
      }

    case HttpGetResult(url, Success(body)) =>
      master ! CrawlResult(url, parser.links(body))

    case HttpGetResult(url, Failure(error)) =>
      master ! CrawlResult(url, List.empty)
  }
}
