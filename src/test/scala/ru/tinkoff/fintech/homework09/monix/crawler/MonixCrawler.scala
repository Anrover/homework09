package ru.tinkoff.fintech.homework09.monix.crawler

import monix.eval.Task
import org.scalatest.{AsyncFlatSpec, FlatSpec, Matchers}
import monix.execution.Scheduler.Implicits.global

class MonixCrawlerSpec extends AsyncFlatSpec with Matchers {
  val http: Http = new Http {
    val internet = Map[Url,Body](
      url("host0", "path0") -> List(url("host0", "path0"), url("host0", "path1"), url("host1", "path0")),
      url("host1", "path0") -> List(url("host1", "path1"), url("host2", "path0"), url("host3", "path0"))
    )

    override def get(url: Url): Task[Body] = Task (
      internet.getOrElse(url, List.empty)
    )
  }

  val parser: Parsr = new Parsr {
    override def links(page: Body): List[Url] = page
  }

  val expectedResult = Map[String, Int]("host0" -> 2, "host1" -> 2, "host3" -> 1, "host2" -> 1)

  "MonixCrawler" should "correct work" in {
    new CrawlRoutines(http, parser).crawl(url("host0", "path0")).runToFuture map {result =>
      result should be (expectedResult)
    }
  }
}
