package com.example.pokerlake.shared

import Ordering.Implicits._
import cats._
import cats.implicits._
import com.example.pokerlake.shared.game._
import com.example.pokerlake.shared.game.Rank._

object PokerHand {

  val handTypeShift = 24

  sealed trait PokerHand extends Ordered[PokerHand] {
    val handType: Int
    def baseValue: Int = handType << handTypeShift
    val handValue: Int
    def compare(that: PokerHand): Int = this.handValue compare that.handValue
    def serialize: String
  }

  case class StraightFlush(suit: Suit, straight: Straight) extends PokerHand {
    override val handType: Int   = 10
    override lazy val handValue: Int = baseValue + straight.highCard.highVal

    // def compare(that: StraightFlush): Int = this.straight.highCard compare that.straight.highCard
    lazy val serialize: String =
      s"""${StraightFlush.abbr},${suit.literal}${straight.ranks
        .map(_.literal)
        .mkString}"""
  }
  object StraightFlush {
    val abbr = "sf"
  }

  implicit val straightFlushShow =
    Show.show[StraightFlush]((f: StraightFlush) => s"Straightflush of ${f.suit}, ${f.straight.highCard} high")

  case class Quads(rank: Rank, kickers: Ranks) extends PokerHand {
    override val handType: Int   = 9
    override lazy val handValue: Int = baseValue + (rank.highVal << 16) + kickers.value
    lazy val serialize: String =
      s"""${Quads.abbr},${rank.literal}${kickers.map(_.literal).mkString}"""
  }
  object Quads {
    val abbr = "qu"
  }

  implicit val quadsShow =
    Show.show[Quads]((q: Quads) => s"Four of a kind ${q.rank}s with ${q.kickers.show} kicker")
  implicit val quadsOrdering: Ordering[Quads] =
    Ordering.by(q => q.rank :: q.kickers)

  case class FullHouse(rank3: Rank, rank2: Rank) extends PokerHand {
    override val handType: Int   = 8
    override lazy val handValue: Int = baseValue + (rank3.highVal << 8) + rank2.highVal
    lazy val serialize: String =
      s"""${FullHouse.abbr},${rank3.literal}${rank2.literal}"""
  }
  object FullHouse {
    val abbr = "fh"
  }

  // implicit val fullHouseOrdering: Ordering[FullHouse] = Ordering.by(f => (f.rank3, f.rank2))  // does not work
  implicit val fullHouseOrdering: Ordering[FullHouse] =
    Ordering.by(f => List(f.rank3, f.rank2)) // with List yes
  implicit val fullHouseShow =
    Show.show[FullHouse]((f: FullHouse) => s"Fullhouse ${f.rank3}s full of ${f.rank2}s")

  case class Flush(suit: Suit, ranks: Ranks) extends PokerHand {
    override val handType: Int   = 7
    override lazy val handValue: Int = baseValue + ranks.value

    def compare(that: Flush): Int = this.ranks compare that.ranks
    lazy val serialize: String =
      s"""${Flush.abbr},${suit.literal}${ranks.map(_.literal).mkString}"""
  }
  object Flush {
    val abbr = "fl"
  }

  implicit val flushShow =
    Show.show[Flush]((f: Flush) => s"Flush of ${f.suit}s (${f.ranks.max} high)")

  case class Straight(ranks: Ranks, highCard: Rank, lowCard: Rank) extends PokerHand {
    override val handType: Int   = 6
    override lazy val handValue: Int = baseValue + highCard.highVal

    lazy val intRanks: Set[Int] = ranks.map(_.highVal).toSet
    lazy val serialize: String =
      s"""${Straight.abbr},${ranks.map(_.literal).mkString}"""
  }

  implicit val straightShow =
    Show.show[Straight]((f: Straight) => s"Straight from ${f.lowCard} to ${f.highCard}")
  implicit val straightOrdering: Ordering[Straight] = (a: Straight, b: Straight) => a.compare(b)

  object Straight {
    // implicit def rw: RW[FiveHand] = macroRW
    def apply(string: String): Straight = {
      require(string.size == 5, s"Must contain five cards (has ${string.length} cards, ${string})")
      require(string.forall(charRank.contains(_)), s"Contains invalid character ${string}")
      // TODO: require: it is really a straight
      val rs: Ranks = string.toList.map(Rank(_)).desc
      Straight(rs, rs.head, rs.last)
    }

    def apply(ranks: Ranks): Straight = {
      Straight(ranks, ranks.desc.head, ranks.desc.last)
    }
    val abbr = "st"
  }

  val straights: List[Straight] =
    (Ace :: allRanks).sliding(5).toList.map(li => Straight(li))

  case class ThreeOfAKind(rank: Rank, kickers: Ranks) extends PokerHand {
    override val handType: Int   = 5
    override lazy val handValue: Int = baseValue + kickers.value

    lazy val serialize: String =
      s"""${ThreeOfAKind.abbr},${rank.literal}${kickers
        .map(_.literal)
        .mkString}"""
  }
  object ThreeOfAKind {
    val abbr = "th"
  }

  implicit val threeShow =
    Show.show[ThreeOfAKind]((t: ThreeOfAKind) => s"Three of a kind ${t.rank}s with ${t.kickers.show} kicker")

  case class TwoPair(highRank: Rank, lowRank: Rank, kickers: Ranks) extends PokerHand {
    override val handType: Int = 4
    override lazy val handValue
      : Int = baseValue + (highRank.highVal << 16) + (lowRank.highVal << +8) + kickers.value
    lazy val serialize: String =
      s"""${TwoPair.abbr},${highRank.literal}${lowRank.literal}${kickers
        .map(_.literal)
        .mkString}"""
  }
  object TwoPair {
    val abbr = "tp"
  }

  implicit val twoPairShow = Show.show[TwoPair]((t: TwoPair) =>
    s"Two pair ${t.highRank}s and ${t.lowRank}s with ${t.kickers.show} kicker")

  case class Pair(rank: Rank, kickers: Ranks) extends PokerHand {
    override val handType: Int   = 3
    override lazy val handValue: Int = baseValue + (rank.highVal << 16) + kickers.value

    def compare(that: Pair) = {
      val tmp0 = this.rank compare that.rank
      if (tmp0 == 0) {
        this.kickers compare that.kickers
      } else tmp0
    }
    lazy val serialize: String =
      s"""${Pair.abbr},${rank.literal}${kickers.map(_.literal).mkString}"""
  }
  object Pair {
    val abbr = "pa"
  }

  implicit val pairShow = Show.show[Pair]((t: Pair) => s"Pair of ${t.rank}s with ${t.kickers.show} kicker")

  case class HighCard(rank: Rank, kickers: Ranks) extends PokerHand {
    override val handType: Int   = 2
    override lazy val handValue: Int = baseValue + kickers.value

    def compare(that: HighCard) = {
      val tmp0 = this.rank compare that.rank
      if (tmp0 == 0) {
        this.kickers compare that.kickers
      } else tmp0
    }
    lazy val serialize: String =
      s"""${HighCard.abbr},${rank.literal}${kickers.map(_.literal).mkString}"""
  }
  object HighCard {
    lazy val abbr = "hc"
  }

  implicit val highCardShow: Show[HighCard] =
    Show.show[HighCard]((t: HighCard) => s"Highcard ${t.rank} with ${t.kickers.show} kicker")

  // how do I avoid to write this down?!
  implicit val pokerHandShow: Show[PokerHand] =
    Show.show[PokerHand] {
      case t: HighCard      => t.show
      case t: Pair          => t.show
      case t: TwoPair       => t.show
      case t: ThreeOfAKind  => t.show
      case t: Straight      => t.show
      case t: Flush         => t.show
      case t: FullHouse     => t.show
      case t: Quads         => t.show
      case t: StraightFlush => t.show
    }

  /*
  implicit val pokerHandOrdering: Ordering[PokerHand] =
    (hero: PokerHand, villain: PokerHand) => {
      // 1 for win, 0 for tie, -1 for loss
      if (hero.handStrength == villain.handStrength) {
        (hero, villain) match {
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
      } else hero.handStrength compare villain.handStrength
    }
    */

  /*
  def fromHandValue(handValue: Int): PokerHand = {
    val handType = handValue >> handTypeShift
  }
  */
}
