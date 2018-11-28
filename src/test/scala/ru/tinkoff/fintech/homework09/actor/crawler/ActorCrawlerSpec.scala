package ru.tinkoff.fintech.homework09.actor.crawler

import akka.actor.{ActorRef, ActorSystem, Props}
import org.scalatest.{AsyncFlatSpec, FlatSpec, Matchers}

import scala.concurrent.{Future, Promise}

class ActorCrawlerSpec extends AsyncFlatSpec with Matchers {
  val http: Http = new Http {
    val internet: Map[Url, Body] = Map[Url,Body](
      url("host0", "path0") -> List(url("host0", "path0"), url("host0", "path1"), url("host1", "path0")),
      url("host1", "path0") -> List(url("host1", "path1"), url("host2", "path0"), url("host3", "path0"))
    )

    override def get(url: Url): Future[Body] = Future.successful(
      internet.getOrElse(url, List.empty)
    )
  }

  val parser: Parsr = new Parsr {
    override def links(page: Body): List[Url] = page
  }

  def wrkFactory(manager: ActorRef): Props = Props(new Worker(http, parser, manager))

  val system = ActorSystem("myactorz")

  val result = Promise[Map[Host, Int]]()
  val expectedResult = Map[String, Int]("host0" -> 2, "host1" -> 2, "host3" -> 1, "host2" -> 1)
  system.actorOf(Props(new Manager(wrkFactory, result))) ! Start(url("host0", "path0"))

  import system.dispatcher
  "ActorCrawler" should "correct work" in {
    result.future map { x =>
      x should be (expectedResult)
    }
  }
}

