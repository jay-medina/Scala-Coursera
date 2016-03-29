package com.mrfunkycold.demo.week3.hw

class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"

  def textContains(word: String) = text.contains(word)
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    if(this.isEmpty) acc
    else if(p(this.head)) tail.filterAcc(p, acc.incl(head))
    else tail.filterAcc(p, acc)
  }

  def union(that: TweetSet): TweetSet = this.filterAcc(_ => true, that)

  def mostRetweeted: Tweet

  def descendingByRetweet: TweetList = {
    if(this.isEmpty) Nil
    else {
      val m = this.mostRetweeted

      new Cons(m, this.remove(m).descendingByRetweet)
    }
  }

  def isEmpty: Boolean
  def head: Tweet = throw new Error("Error.head")
  def tail: TweetSet = throw new Error

  def incl(tweet: Tweet): TweetSet

  def remove(tweet: Tweet): TweetSet

  def contains(tweet: Tweet): Boolean

  def length: Int

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {
  def isEmpty = true
  def mostRetweeted = throw new NoSuchElementException

  def length: Int = 0

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def isEmpty = false
  override def head: Tweet = if(left.isEmpty) elem else left.head
  override def tail: TweetSet = if(left.isEmpty) right else new NonEmpty(elem, left.tail, right)

  def mostRetweeted: Tweet = mostRetweetedHelper(elem)

  def mostRetweetedHelper(m: Tweet): Tweet = {
    if(tail.isEmpty) m
    else {
      val t = tail.mostRetweeted
      if(t.retweets > m.retweets) t
      else m
    }
  }

  def length: Int = 1 + left.length + right.length
    
  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def length: Int
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
  def length = 0
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
  def length: Int = 1 + tail.length
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val allTweets: TweetSet = TweetReader.allTweets
  lazy val googleTweets: TweetSet = allTweets.filter(tweet => google.exists(tweet.textContains))
  lazy val appleTweets: TweetSet = allTweets.filter(tweet => apple.exists(tweet.textContains))
  lazy val gAndaTweets = allTweets.filter(tweet => google.exists(tweet.textContains) && apple.exists(tweet.textContains))
  lazy val googleAndApple: TweetSet = googleTweets union appleTweets
  
  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = googleAndApple.descendingByRetweet
  }

object Main extends App {
  // Print the trending tweets
  //GoogleVsApple.trending foreach println
  println (GoogleVsApple.allTweets.length)
  println (GoogleVsApple.googleTweets.length)
  println (GoogleVsApple.appleTweets.length)
  println (GoogleVsApple.googleAndApple.length)
  println (GoogleVsApple.gAndaTweets.length)
  println (GoogleVsApple.trending.length)
  GoogleVsApple.trending foreach println

}
