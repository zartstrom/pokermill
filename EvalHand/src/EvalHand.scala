package com.example.pokerlake.shared

import cats._
import cats.data._
import cats.implicits._
import Const._
import PokerHand._
import com.example.pokerlake.shared.game._
import com.example.pokerlake.shared.game.Rank._
import game.{Deuce, Rank, Suit}
import game.Suit._

object EvalHand {

  def getStraightFlush(intRanks: Set[Int], flushSuits: List[Suit])(
      cards: Cards): Option[StraightFlush] = {
    // val cardRanks: Set[Rank] = cards.map(_.rank).toSet
    if (intRanks.size < 5 || flushSuits == Nil) return None
    // val hasFlush = flushSuits.nonEmpty
    val myStraights = straights.filter((s: Straight) => {
      //val rs: Set[Int] = s.ranks.toSet.map(_.highVal)
      s.intRanks.subsetOf(intRanks)
    })
    //val flushSuits: Map[Suit, Int] =
    //  cards.map(_.suit).groupBy(x => x).mapValues(_.size).filter(_._2 >= 5)

    if (myStraights.nonEmpty && flushSuits.nonEmpty) {
      val suit = flushSuits.head // there can be only one flush

      val sfs = myStraights.filter(straight => {
        straight.ranks.forall(rank => cards.contains(Card(rank, suit)))
      })
      if (sfs.nonEmpty) {
        Some(StraightFlush(suit, sfs.max))
      } else None
    } else None
  }
  def isStraightFlush(flushSuits: List[Suit])(cards: Cards): Boolean = {
    if (flushSuits == Nil) return false
    val suit = flushSuits.head // there can be only one flush
    val intRanks = cards.filter(_.suit == suit).map(_.rank.highVal).toSet
    isStraight(intRanks)
  }

  def getFlush(flushSuits: List[Suit])(cards: Cards): Option[Flush] = {
    if (flushSuits == Nil) return None
    val countSuits: Map[Suit, Int] =
      cards.map(_.suit).groupBy(x => x).mapValues(_.size)

    if (countSuits.count(x => x._2 >= 5) == 1) { // there can only be one flush
      val suit = countSuits.head._1 // bug here
      val ranks: Ranks = cards.filter(_.suit == suit).map(_.rank).desc.take(5)
      Some(Flush(suit, ranks))
    } else None
  }
  def isFlush(flushSuits: List[Suit]): Boolean = flushSuits != Nil

  def getStraight(intRanks: Set[Int])(cards: Cards): Option[Straight] = {
    //val cardRanks: Set[Rank] = cards.map(_.rank).toSet
    val myStraights = straights.filter((s: Straight) => {
      s.intRanks.subsetOf(intRanks)
    })
    if (myStraights.nonEmpty) {
      Some(myStraights.max)
    } else None
  }
  val intStraights = 1.to(14).sliding(5).toList
  def isStraight(intRanks: Set[Int]): Boolean = {
    if (intRanks.size < 5) return false
    val intRanksWithAce = if (intRanks.contains(14)) {
      intRanks + 1
    } else intRanks

    // foldLeft zero: (current streak, max streak)
    val maxLengthStraight = 1.to(14)
      .foldLeft((0, 0))((zt, b) =>
        if (intRanksWithAce.contains(b)) (zt._1 + 1, math.max(zt._1 + 1, zt._2))
        else (0, zt._2))._2
    maxLengthStraight >= 5
  }

  def isRankHand(handThreshold: List[Int])(rankCountList: List[Int]): Boolean = {
    rankCountList >= handThreshold
  }

  def getQuads(rankCount: Map[Rank, Int])(cards: Cards): Option[Quads] = {
    // val rankCount: Map[Rank, Int] = cards.groupBy(_.rank).mapValues(_.length)
    if (rankCount.values.max < 4) return None
    rankCount
      .find(x => x._2 == 4)
      .map(t => {
        val quadsRank = t._1
        val kicker = cards.map(_.rank).filter(_ != quadsRank).max
        Quads(quadsRank, List(kicker))
      })
  }
  def isQuads(rankCountList: List[Int]): Boolean = isRankHand(List(4))(rankCountList)

  def getFullHouse(rankCount: Map[Rank, Int])(
      cards: Cards): Option[FullHouse] = {
    val rankCounts3 = rankCount.filter(_._2 == 3).keys
    val rankCounts2 = rankCount.filter(_._2 >= 2).keys
    if (rankCounts3.nonEmpty && rankCounts2.size >= 2) {
      val threeRank = rankCounts3.max
      val twoRank = rankCounts2.filter(_ != threeRank).max
      Some(FullHouse(threeRank, twoRank))
    } else None

  }
  def isFullHouse(rankCountList: List[Int]): Boolean = isRankHand(List(3, 2))(rankCountList)

  def getThreeOfAKind(rankCount: Map[Rank, Int])(
      cards: Cards): Option[ThreeOfAKind] = {
    val rs = rankCount.filter(x => x._2 == 3).keys
    if (rs.nonEmpty) {
      val rank = rankCount.filter(x => x._2 == 3).keys.max
      val kickers: Ranks = cards.map(_.rank).filter(_ != rank).desc.take(2)
      Some(ThreeOfAKind(rank, kickers))
    } else None
  }
  def isThreeOfAKind(rankCountList: List[Int]): Boolean = isRankHand(List(3))(rankCountList)

  def getTwoPair(rankCount: Map[Rank, Int])(cards: Cards): Option[TwoPair] = {
    val rankCounts2: List[Rank] = rankCount.filter(_._2 >= 2).keys.toList
    if (rankCounts2.size >= 2) {
      val highRank = rankCounts2.sorted.reverse.head
      val lowRank = rankCounts2.sorted.reverse.tail.head
      val kickers = rankCount.keys
        .filter(r => (r != highRank) && (r != lowRank))
        .toList
        .sorted
        .reverse
        .take(1)
      Some(TwoPair(highRank, lowRank, kickers))
    } else None
  }
  def isTwoPair(rankCountList: List[Int]): Boolean = isRankHand(List(2, 2))(rankCountList)

  def highCard(cards: Cards): Rank = {
    cards.map(_.rank).max
  }

  def getPair(rankCount: Map[Rank, Int])(cards: Cards): Option[Pair] = {
    // val rankCount: Map[Rank, Int] = cards.groupBy(_.rank).mapValues(_.length)
    //rankCount.values.foldLeft(1)((x: Int, y: Int) => math.max(x, y))
    val rs = rankCount.filter(x => x._2 == 2).keys
    if (rs.nonEmpty) {
      val rank = rs.max
      Some(Pair(rank, cards.map(_.rank).filter(_ != rank).desc.take(3)))
    } else None
  }
  def isPair(rankCountList: List[Int]): Boolean = isRankHand(List(2))(rankCountList)

  def getHighCard(cards: Cards): Option[HighCard] = {
    val ranks = cards.map(_.rank).sorted
    val rank = ranks.head
    val kickers: Ranks = ranks.take(5).tail
    Some(HighCard(rank, kickers))
  }
  def isHighCard = true

  def eval(cards: Cards): PokerHand = {
    lazy val intRanks: Set[Int] = cards.map(_.rank.highVal).toSet
    lazy val isFlush: Boolean = cards.map(_.suit.suitNumber).groupBy(x => x).mapValues(_.size).filter(_._2 >= 5).nonEmpty
    lazy val flushSuits: List[Suit] =
      cards
        .map(_.suit)
        .groupBy(x => x)
        .mapValues(_.size)
        .filter(_._2 >= 5)
        .keys
        .toList
    lazy val rankCount: Map[Rank, Int] =
      cards.groupBy(_.rank).mapValues(_.length)
    lazy val rankCountList = rankCount.values.toList.sorted.reverse
    // val isStraight =k

    lazy val isSF = isFlush && isStraightFlush(flushSuits)(cards)
    lazy val isQu = isQuads(rankCountList)
    lazy val isFh = isFullHouse(rankCountList)
    lazy val isFl = isFlush
    lazy val isSt = isStraight(intRanks)
    lazy val isTh = isThreeOfAKind(rankCountList)
    lazy val isTp = isTwoPair(rankCountList)
    lazy val isPa = isPair(rankCountList)
    lazy val isHc = isHighCard

    lazy val stream = Stream(
      (isSF, getStraightFlush(intRanks, flushSuits) _),
      (isQu, getQuads(rankCount) _),
      (isFh, getFullHouse(rankCount) _),
      (isFl, getFlush(flushSuits) _),
      (isSt, getStraight(intRanks) _),
      (isTh, getThreeOfAKind(rankCount) _),
      (isTp, getTwoPair(rankCount) _),
      (isPa, getPair(rankCount) _),
      (isHc, getHighCard _),
    )
    //val handOpt: Option[PokerHand] = stream.filter(t => t._1).map({t =>
    val hand: PokerHand = stream.filter(t => t._1).map({t =>
      val opt = t._2(cards)
      opt match {
        case Some(pokerHand: PokerHand) => pokerHand
        case None => { println(cards); HighCard(Deuce, List()) }
      }
    }).head

    hand

    /*
    val betterImpl = Stream(
      getStraightFlush(intRanks, flushSuits) _,
      getQuads(rankCount) _,
      getFullHouse(rankCount) _,
      getFlush(flushSuits) _,
      getStraight(intRanks) _,
      getThreeOfAKind(rankCount) _,
      getTwoPair(rankCount) _,
      getPair(rankCount) _,
      getHighCard _
    )
    betterImpl.flatMap((f: Cards => Option[PokerHand]) => f(cards)).head
    */
  }

  // move it to test
  //object Test {
  val cs01 = Cards("AcAsKcTc8h")
  val cs02 = Cards("9c5s5cTc5h")
  val cs03 = Cards("AcAsAsTcAh")
  val cs04 = Cards("AcKcTc6c5c")
  val cs05 = Cards("3c4c5s6c7d")
  val cs06 = Cards("QcQdQhJdJs")
  val cs07 = Cards("3c4c5c6c7d8cJc")
  val cs08 = Cards("3c4c5c6c7c8c9cJc")
  val hands = List(cs01, cs02, cs03, cs04, cs05, cs06, cs07, cs08)
  // hands.foreach(x => println(x.show + "    " + EvalHand.eval(x).show))

  def handCompare(hero: PokerHand, villian: PokerHand): Int = {
    // 1 for win, 0 for tie, -1 for loss
    if (hero.handType == villian.handType) {
      (hero, villian) match {
        case (h: HighCard, v: HighCard)           => h compare v
        case (h: Pair, v: Pair)                   => h compare v
        case (h: TwoPair, v: TwoPair)             => h compare v
        case (h: ThreeOfAKind, v: ThreeOfAKind)   => h compare v
        case (h: Straight, v: Straight)           => h compare v
        case (h: Flush, v: Flush)                 => h compare v
        case (h: FullHouse, v: FullHouse)         => fullHouseOrdering.compare(h, v)
        case (h: Quads, v: Quads)                 => h compare v
        case (h: StraightFlush, v: StraightFlush) => h compare v
        case _                                    => 0
      }
    } else hero.handType compare villian.handType
  }

  case class Evaluation(wins: Int, losses: Int, ties: Int)

  def h(t: (Int, Int, Int), r: Int): (Int, Int, Int) = {
    t match {
      case (x, y, z) =>
        if (r == 1) (x + 1, y, z)
        else if (r == -1) (x, y + 1, z)
        else (x, y, z + 1)
    }
  }

  /*
  def getEval(cardInts: List[Int]): PokerHand = {
    // val resultOpt = EvalFiveCards.allHands.get(EvalFiveCards.keyLong(cardInts.sorted(descOrdering)))
    val resultOpt = FiveCards.allHands.get(cardInts.sorted(descOrdering))
    resultOpt match {
      case Some(pokerHand) => pokerHand
      case None =>
        HighCard(Deuce, List()) // every five carded Hand must be in allHands
    }
  }

  def pickBestHand(cardInts: List[Int]): PokerHand = {
    // require(cards.length >= 5)
    cardInts.combinations(5).map(getEval).max
  }

  def playOut(cardsHero: Cards, cardsVillain: Cards): Evaluation = {
    val intsHero = cardsHero.map(_.toInt)
    val intsVillain = cardsVillain.map(_.toInt)
    val remainingCards = Cards.allCards.filter(c =>
      !cardsHero.contains(c) && !cardsVillain.contains(c))
    assert(remainingCards.length == 52 - cardsHero.length - cardsVillain.length)
    val t = remainingCards.map(_.toInt)
      .combinations(5)
      .map(cs =>
        handCompare(pickBestHand(intsHero ++ cs),
                    pickBestHand(intsVillain ++ cs)))
      .foldLeft((0, 0, 0))(h)
    Evaluation(t._1, t._2, t._3)
  }
  */

}
