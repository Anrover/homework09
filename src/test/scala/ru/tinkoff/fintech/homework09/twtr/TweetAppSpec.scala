package ru.tinkoff.fintech.homework09.twtr

import org.scalatest.Inside._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{AsyncFlatSpec, Matchers}

class TweetAppSpec extends AsyncFlatSpec with Matchers with ScalaFutures {
  val text1 = "Hello, Vasya #qwe#abc dfsaf #qwer123@mail_ru.com"
  val userName = "vova"

  "TwitterApi" should "correct create tweet" in {
    val tweetApi = new TwitterApi(new LocalStorage)
    tweetApi.createTweet(CreateTweetRequest(text1, userName)).map { successTweet =>
        successTweet should matchPattern { case Success(_) => }
        inside (successTweet) { case Success(tweet) =>
          tweet should matchPattern { case Tweet(_, `userName`,
          `text1`, Seq("#qwe", "#abc", "#qwer123@mail_ru"), _, 0) => }
        }
    }
  }
  it should "return error, when create tweet, if length Text > MaxLenTweet" in {
    val tweetApi = new TwitterApi(new LocalStorage)
    val messageError = "The text length has been exceeded"
    val bigText: String = "abc" * 300

    tweetApi.createTweet(CreateTweetRequest(bigText, userName)) map {response =>
      response should matchPattern { case Error(_) => }
      inside(response) { case Error(message) => message should be (messageError) }
    }
  }

  it should "correct like tweet" in {
    val tweetApi = new TwitterApi(new LocalStorage)
    whenReady(tweetApi.createTweet(CreateTweetRequest(text1, userName))) {
      case Success(tweet) =>
        tweetApi.likeTweet(LikeRequest(tweet.id)) map { response => {
          inside(response) {
            case Success(tweetAfterLike) =>
              tweetAfterLike should be (tweet.copy(likes = tweet.likes + 1))
          }}
        }
      case _ => throw new Exception("Something went wrong")
    }
  }
  it should "return error, when like tweet, if tweet with this id does not exist in Storage" in {
    val tweetApi = new TwitterApi(new LocalStorage)
    val nonexistentId = "123"
    val messageError = s"No tweet for id: $nonexistentId"

    tweetApi.likeTweet(LikeRequest(nonexistentId)) map { response =>
      response should matchPattern { case Error(_) => }
      inside(response) { case Error(message) => message should be (messageError) }
    }
  }

  it should "correct return tweet by id" in {
    val tweetApi = new TwitterApi(new LocalStorage)

    whenReady(tweetApi.createTweet(CreateTweetRequest(text1, userName))) {
      case Success(tweet) =>
        tweetApi.getTweet(GetTweetRequest(tweet.id)) map { successTweet2 =>
          successTweet2 should matchPattern { case Success(_) => }
          inside(successTweet2) { case Success(tweet2) => tweet2 should be (tweet) }
        }
      case _ => throw new Exception("Something went wrong")
    }
  }
  it should "return error, when getting tweet, if tweet with this id does not exist in Storage" in {
    val tweetApi = new TwitterApi(new LocalStorage)
    val nonexistentId = "1244121"
    val messageError = s"No tweet for id: $nonexistentId"

   tweetApi.getTweet(GetTweetRequest(nonexistentId)) map {response =>
     response should matchPattern { case Error(_) => }
     inside(response) { case Error(message) => message should be (messageError) }
   }
  }
  it should "correct work if network is dropped" in {
    val tweetApi = new TwitterApi(new LocalStorage)
    val messageError = "The time to access the server is out"
    tweetApi.makeBedRequest map {result =>
      result should matchPattern { case Error(_) => }
      inside(result) { case Error(message) => message should be (messageError) }
    }
  }
}
