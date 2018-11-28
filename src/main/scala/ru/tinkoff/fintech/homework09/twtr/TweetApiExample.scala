package ru.tinkoff.fintech.homework09.twtr

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import java.time.Instant
import java.util.UUID.randomUUID

import scala.concurrent.Future

/**
  * Вам необходимо реализовать api для создания твиттов, получения твитта и лайка твитта
  *
  * Создание твитта:
  * На вход к вам поступает CreateTweetRequest, из которого вы должны создать объект Tweet, обязательно проверив длину
  * текста (а может быть потом появятся и другие проверки).
  * hashTags вычисляется из tweet.text, собирая все слова, начинающиеся с символа `#`.
  * tweet.id генерируется из `UUID.randomUUID.toString`.
  * При условии прохождения всех проверок твит сохраняется в базу данных (вы можете реализовать любой способ сохранения:
  * в памяти, запись в файл, или то, что вам захочется).
  * После выполнения всех операций должен вернуться созданный объект.
  *
  * Получение твитта:
  * На вход к вам поступает GetTweetRequest, вернуть вы должны объект tweet, если он найдем по id.
  *
  * Лайк твитта:
  * Должен обновлять количество лайков у твитта и возвращать новое значение.
  * Если твит не найдет, то должна возвращаться ошибка
  *
  *
  * Все функции должны возвращать значение типа Future[?]
  *
  *
  * Если же сложилось непоправимое(например обрушилась сеть),
  * то необходимо обработать это достойным образом.
  */

sealed trait Result[+T]
final case class Success[T](result: T) extends Result[T]
final case class Error[T](message: String) extends Result[T]

case class Tweet(id: String,
                 user: String,
                 text: String,
                 hashTags: Seq[String] = Seq.empty,
                 createdAt: Option[Instant] = None,
                 likes: Int)

case class CreateTweetRequest(text: String, user: String)
case class GetTweetRequest(id: String)
case class LikeRequest(id: String)

object FutureTimeout {

  import scala.concurrent.ExecutionContext.Implicits.global

  implicit class FutureTimeoutLike[T](f: Future[Result[T]]) {
    def withTimeout(ms: Long): Future[Result[T]] = Future.firstCompletedOf(List(f, Future {
      blocking {
        Thread.sleep(ms)
      }
      Error("The time to access the server is out")
    }))

    val withTimeout: Future[Result[T]] = withTimeout(2000)
  }
}

trait TweetStorage {
  def putTweet(tweet: Tweet): Future[Result[Tweet]]
  def getTweet(id: String): Future[Result[Tweet]]
  def updateTweet(tweet: Tweet): Future[Result[Tweet]]
}

class LocalStorage extends TweetStorage{
  private var tweets: Map[String, Tweet] = Map.empty
  override def putTweet(tweet: Tweet): Future[Result[Tweet]] = Future {
    tweets.get(tweet.id) match {
      case Some(_) => Error(s"A tweet with this id: ${tweet.id} is already in storage")
      case None =>
        tweets += (tweet.id -> tweet)
        Success(tweet)
    }
  }

  override def getTweet(id: String): Future[Result[Tweet]] = Future {
    tweets.get(id) match {
      case Some(tweet) => Success(tweet)
      case _ => Error(s"No tweet for id: $id")
    }
  }

  override def updateTweet(tweet: Tweet): Future[Result[Tweet]] = Future {
    tweets.get(tweet.id) match {
      case Some(_) =>
        tweets += (tweet.id -> tweet)
        Success(tweet)
      case None => Error(s"A tweet with this id: ${tweet.id} is not in storage")
    }
  }
}

class TwitterApi(storage: TweetStorage) {
  import FutureTimeout._
  private val MaxLenTweet = 280
  private val RegExHashTags = "#[A-Za-z_@0-9]+".r

  def createTweet(request: CreateTweetRequest): Future[Result[Tweet]] = request match {
    case CreateTweetRequest(text, _) if text.length > MaxLenTweet =>
      Future(Error("The text length has been exceeded"))
    case CreateTweetRequest(text, user) =>
      storage.putTweet(Tweet(randomUUID().toString, user, text,
        getHashTags(text), Some(Instant.now), 0)) withTimeout
  }

  private def getHashTags(text: String): Seq[String] =
    RegExHashTags.findAllIn(text).toSeq

  def likeTweet(request: LikeRequest): Future[Result[Tweet]] =
    storage.getTweet(request.id) flatMap  {
      case Success(tweet) =>
        storage.updateTweet(tweet.copy(likes = tweet.likes + 1)) withTimeout
      case Error(message) => Future(Error(message))
    }

  def getTweet(request: GetTweetRequest): Future[Result[Tweet]] =
    storage.getTweet(request.id) withTimeout

  //Для теста сбоя сети
  def makeBedRequest: Future[Result[Tweet]] = Future {
    Thread.sleep(5000)
    Success(Tweet("1", "Vova", "firstTweetVova", likes = 0))
  } withTimeout 1000
}

object TweetApiExample extends App {
  val storage: TweetStorage = new LocalStorage()
  val app = new TwitterApi(storage)

  val request = CreateTweetRequest(user = "me", text = "Hello, world!")

  val resF = app.createTweet(request)

  resF.foreach {
    case Success(value) => println(s"Created tweet with id: ${value.id}")
    case Error(message) => println(s"Failed to create tweet: $message")
  }
  println(Await.result(resF, 10 seconds))
//  try {
//    println(Await.result(resF, 10 seconds))
//  } catch {
//    case _: TimeoutException => println("The time to access the server is out")
//  }
  // конечно-же программа должна что-то напечатать в stdout
}