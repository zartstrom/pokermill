package com.example.pokerlake.shared

import java.io._
import scala.collection.mutable

import com.example.pokerlake.shared.Const.Card

import scala.io.Source
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.duration._
import com.redis._
import com.redis.serialization.Parse.Implicits._

// the following is equivalent to `implicit val ec = ExecutionContext.global`
import scala.concurrent.ExecutionContext.Implicits.global
// import scala.util.{Failure, Success}

import Const.Cards
import Const.descOrdering
import Const.{RANK_SHIFT, SUIT_BITS}
import com.example.pokerlake.shared.game._
import PokerHand._

object FiveCards {
  private def getRankInt(c: Int): Int = c >> RANK_SHIFT
  private def getSuitInt(c: Int): Int = c & SUIT_BITS

  def toHighCard(ints: List[Int]): HighCard = {
    val intRanks = ints.map(getRankInt)
    HighCard(Rank(intRanks.head), intRanks.tail.map(Rank(_)))
  }

  def toPair(ints: List[Int]): Pair = {
    val intRanks = ints.map(getRankInt)
    val twoRank =
      intRanks.groupBy(x => x).mapValues(_.size).filter(_._2 == 2).keys.head
    val kickers = intRanks.filter(_ != twoRank).map(Rank(_))
    Pair(Rank(twoRank), kickers)
  }

  def toTwoPair(ints: List[Int]): TwoPair = {
    val intRanks = ints.map(getRankInt)
    val twoRanks =
      intRanks.groupBy(x => x).mapValues(_.size).filter(_._2 == 2).keys
    val kickers = intRanks.filter(!twoRanks.toSet.contains(_)).map(Rank(_))
    TwoPair(Rank(twoRanks.max), Rank(twoRanks.min), kickers)
  }

  def toThreeOfAKind(ints: List[Int]): ThreeOfAKind = {
    val intRanks = ints.map(getRankInt)
    val threeRank =
      intRanks.groupBy(x => x).mapValues(_.size).filter(_._2 == 3).keys.head
    val kickers = intRanks.filter(_ != threeRank).map(Rank(_))

    ThreeOfAKind(Rank(threeRank), kickers)
  }

  def toStraight(ints: List[Int]): Straight = {
    val intRanks = ints.map(getRankInt)
    Straight(intRanks.map(Rank(_)), Rank(intRanks.max), Rank(intRanks.min))
  }

  def toFlush(ints: List[Int]): Flush = {
    val suit = Suit(getSuitInt(ints.head))
    Flush(suit, ints.map(x => Rank(getRankInt(x))))
  }

  def toFullHouse(ints: List[Int]): FullHouse = {
    val intRanks = ints.map(getRankInt)
    val a        = intRanks.toSet.head
    val b        = intRanks.toSet.tail.head
    if (intRanks.count(_ == a) == 3) {
      FullHouse(Rank(a), Rank(b))
    } else {
      FullHouse(Rank(b), Rank(a))
    }
  }

  def toQuads(ints: List[Int]): Quads = {
    val intRanks = ints.map(getRankInt)
    val a        = intRanks.toSet.head
    val b        = intRanks.toSet.tail.head
    if (intRanks.count(_ == a) == 4) {
      Quads(Rank(a), List(Rank(b)))
    } else {
      Quads(Rank(b), List(Rank(a)))
    }
  }

  def toStraightFlush(ints: List[Int]): StraightFlush = {
    val suit     = Suit(getSuitInt(ints.head))
    val intRanks = ints.map(getRankInt)
    StraightFlush(suit, Straight(intRanks.map(Rank(_)), Rank(intRanks.max), Rank(intRanks.min)))
  }

  def isFlush(cardBytes: List[Int]): Boolean = {
    val bytes = cardBytes.map(getSuitInt)
    bytes.tail.foldLeft(bytes.head)((previous: Int, byte: Int) => {
      if (previous != byte) return false else byte
    })
    true
  }

  def isSimpleStraight(bytes: List[Int]): Boolean = {
    bytes.tail.foldLeft(bytes.head)((previous: Int, byte: Int) => {
      if (previous - 1 != byte) return false else byte
    })
    true
  }

  def isStraight(cardBytes: List[Int]): Boolean = {
    val bytes = cardBytes.map(getRankInt)

    if (bytes.head == 14) {
      isSimpleStraight(bytes) || isSimpleStraight(bytes.tail :+ 1)
    } else isSimpleStraight(bytes)
  }

  def ranks(cardBytes: List[Int]): Int = {
    val bytes = cardBytes.map(getRankInt)
    var arr   = Array.ofDim[Int](5)
    arr(0) = 1
    bytes.tail.foldLeft((0, bytes.head))((positionPrevious: (Int, Int), byte: Int) => {
      val position     = positionPrevious._1
      val previousByte = positionPrevious._2

      if (byte == previousByte) {
        arr(position) += 1
        (position, byte)
      } else {
        arr(position + 1) += 1
        (position + 1, byte)
      }
    })
    arr = arr.sortWith(_ > _)
    // Examples
    // Quads:     arr = [4, 1, 0, 0, 0]
    // FullHouse: arr = [3, 2, 0, 0, 0]
    // TwoPair:   arr = [2, 2, 1, 0, 0]
    // HighCard:  arr = [1, 1, 1, 1, 1]
    arr(0) * 64 + arr(1) * 16 + arr(2) * 4 + arr(3) * 1
  }

  def evalFiveCards(cards: Cards): PokerHand = {
    require(cards.size == 5)
    evalFiveInts(cards.map(_.toInt))
  }

  def evalFiveInts(inBytes: List[Int]): PokerHand = {
    val bytes = inBytes.sorted(descOrdering) // aren't they sorted desc anyway?!
    // require(bytes.size == 5)
    lazy val isFlush: Boolean    = this.isFlush(bytes)
    lazy val isStraight: Boolean = this.isStraight(bytes)
    lazy val ranksValue: Int     = this.ranks(bytes)

    true match {
      case _ if isFlush && isStraight => toStraightFlush(bytes)
      case _ if ranksValue >= 256     => toQuads(bytes)
      case _ if ranksValue >= 224     => toFullHouse(bytes)
      case _ if isFlush               => toFlush(bytes)
      case _ if isStraight            => toStraight(bytes)
      case _ if ranksValue >= 192     => toThreeOfAKind(bytes)
      case _ if ranksValue >= 160     => toTwoPair(bytes)
      case _ if ranksValue >= 128     => toPair(bytes)
      case _                          => toHighCard(bytes)
    }
  }

  type Lookup = Map[String, PokerHand]
  val emptyLookup: Lookup = Map.empty


  lazy val cardsToHand: Map[Cards, PokerHand] = {
    Cards.allCards
      .map(_.toInt)
      .sorted(descOrdering)
      .combinations(5)
      .map(cs => (cs.map(Card(_)), evalFiveInts(cs)))
      .toMap
  }

  def writeToRedis: Unit = {
    val client = new RedisClient("localhost", 6379)
    for ((cards, hand) <- cardsToHand) {
      client.set(cards.toInt, hand.handValue)
    }
  }
  // writeToRedis

  val evalMap: mutable.Map[Int, Int] = mutable.Map.empty
  lazy val redisClient = new RedisClient("localhost", 6379)
  def eval(hand: Int): Int = {
    evalMap.getOrElseUpdate(hand, redisClient.get[Int](hand).get)
  }

  /*
  lazy val allHandsHashed: Map[Int, PokerHand] = {
    Cards.allCards
      .map(_.toInt6bit)
      .sorted(descOrdering)
      .combinations(5)
      .map((cs: List[Int]) => (cs.sorted(descOrdering).map(Card(_).literal).mkString, evalFiveInts(cs)))
      .toMap
  }

  def combIter: Iterator[List[Int]] =
    Cards.allCards
      .map(_.toInt)
      .sorted(descOrdering)
      .combinations(5)
      // .toVector

  // def spliterator(nofParts: Int, itLength: Int): List[Vector[List[Int]]] = {
  def spliterator(nofParts: Int, itLength: Int): List[Iterator[List[Int]]] = {
    val chunk: Int = itLength / nofParts
    // println(chunk)
    0.until(nofParts)
      .map(i => combIter.slice(i * chunk, (i + 1) * chunk))
      .toList
  }

  def splitOfHands(it: Iterator[List[Int]]): Future[Lookup] = {
    Future {
      val lookup = it
        .map((cs: List[Int]) => (cs.sorted(descOrdering), evalFiveInts(cs)))
        .toMap
      lookup
    }
  }

  def method: Lookup = {
    val seqOfLookupsFuture: Seq[Future[Lookup]] =
      spliterator(4, Calc.choose(5, 52)).map(splitOfHands)
    val futureLookup: Future[Lookup] = Future
      .sequence(seqOfLookupsFuture)
      .map(ls => ls.foldLeft(emptyLookup)(_ ++ _))

    /*
    val response = futureLookup map { items =>
      Some(items)
    } recover {
      case timeout: java.util.concurrent.TimeoutException => None
    }
   */
    Await.result(futureLookup, 40 seconds)
  }
  val result = method

  println(result.size)

  val fiveHandsFile = "FiveHands.txt"
  //case class FiveHand(cardsInts: List[Int], pokerHand: PokerHand)
  case class FiveHand(cardsLong: Long, pokerHand: PokerHand)

  val mult: List[Long] =
    List(256L * 256L * 256L * 256L, 256L * 256L * 256L, 256L * 256L, 256L, 1L)
  def keyLong(li: List[Int]): Long = {
    li.zip(mult).map({ case (x, y) => x * y }).sum
  }

  def writeLookupFile: Unit = {
    val writer = new PrintWriter(new File(fiveHandsFile))
    allHandsHashed.foreach({
      case (li: List[Int], ph: PokerHand) =>
        // val cardsLong = keyLong(li)
        // val fiveHand = FiveHand(keyLong(li), ph)
        // writer.write(fiveHand.asJson.noSpaces)
        writer.write(Serde.serialize(li, ph))
        writer.write("\n")
    })
    writer.close()
  }

  def writeToRedis: Unit = {

  }
  // writeLookupFile

  // lazy val allHands: Map[List[Int], PokerHand] = {
  lazy val allHands: Map[List[Int], PokerHand] = {
    //var tuples: List[(List[Int], PokerHand)] = Nil // List[(List[Int], PokerHand)]()
    var tuples
      : List[(List[Int], PokerHand)] = Nil // List[(List[Int], PokerHand)]()

    val source = Source.fromFile(fiveHandsFile)
    for (line <- source.getLines()) {
      // val fiveHand: FiveHand = decode[FiveHand](line).right.get
      // tuples = (fiveHand.cardsLong, fiveHand.pokerHand) :: tuples
      tuples = Serde.deserialize(line) :: tuples
    }
    source.close()
    tuples.toMap
  }
   */

  // (List[Int], PokerHand)
  // println(allHands.keys.size) // should be (2598960)  // choose 5 from 52
  // allHands.take(12).foreach(println)
  // val ev: EvalHand.Evaluation = EvalHand.playOut(Cards("AcAd"), Cards("KcKd"))
  // println(ev)

}
