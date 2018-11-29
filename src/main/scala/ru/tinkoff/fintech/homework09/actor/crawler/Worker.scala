package ru.tinkoff.fintech.homework09.actor.crawler
import akka.actor.{Actor, ActorLogging, ActorRef}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}
import scala.collection.immutable.Queue

class Worker(http: Http, parser: Parsr, master: ActorRef) extends Actor with ActorLogging {
  private var workerIsBusy = false
  private var queueUrls = Queue.empty[Url]

  private def processUrl(): Unit = queueUrls match {
      case tail :+ url if !workerIsBusy =>
        workerIsBusy = true
        queueUrls = tail
        http.get(url).onComplete(result => self ! HttpGetResult(url, result))
      case _ =>
    }

  override def receive: Receive = {
    case Crawl(url) =>
      queueUrls :+= url
      processUrl()

    case HttpGetResult(url, Success(body)) =>
      val links = parser.links(body)
      workerIsBusy = false
      master ! CrawlResult(url, links)
      processUrl()

    case HttpGetResult(url, Failure(error)) =>
      workerIsBusy = false
      master ! CrawlResult(url, List.empty)
      processUrl()
  }
}
