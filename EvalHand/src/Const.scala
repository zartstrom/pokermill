package com.example.pokerlake.shared

import Ordering.Implicits._
import cats._
import cats.data._
import cats.implicits._
import game.{InvalidRank, InvalidSuit, Rank, Suit}
import game.Suit._

object Const {

  val descOrdering: Ordering[Int] = Ordering[Int].reverse

  /*
  trait Desc[B] {
    // def desc(implicit elem2ordered: B => Ordered[B]): B
    def desc: B
  }

  implicit def descRanks(r: Ranks): Desc[Ranks] =
    new Desc[Ranks] {
      def desc = r.sorted.reverse
    }
   */
  val RANK_SHIFT = 2
  val CARD_SHIFT = 6
  val SUIT_BITS  = 3
  val CARD_BITS = 63

  case class Card(rank: Rank, suit: Suit) {
    def isValid: Boolean = (rank != InvalidRank) && (suit != InvalidSuit)

    override def hashCode(): Int = s"${rank.literal}${suit.literal}".hashCode()
    val literal                  = rank.literal + suit.literal

    // lazy val toIntOld: Int = 16 * rank.highVal + suit.suitNumber
    lazy val toInt: Int    = (rank.highVal << RANK_SHIFT) + suit.suitNumber
  }
  implicit val cardShow                     = Show.show[Card]((card: Card) => card.rank.show + card.suit.show)
  implicit val cardOrdering: Ordering[Card] = Ordering.by((c: Card) => c.toInt).reverse

  val invalidCard = Card(InvalidRank, InvalidSuit)

  object Card {
    def apply(string: String): Card = {
      // expect a card description like "Kh"
      require(string.length == 2)
      val fstLetter = string.head
      val sndLetter = string.tail.head

      val rank = Rank.charRank.getOrElse(fstLetter, InvalidRank)
      val suit = charSuit.getOrElse(sndLetter, InvalidSuit)

      Card(rank, suit)
    }
    val intToCards: Map[Int, Card] = Cards.allCards.map(c => (c.toInt, c)).toMap
    def apply(i: Int): Card        = intToCards.getOrElse(i, invalidCard)
  }

  case class HashCards(cards: List[Card]) {
    override def hashCode(): Int = cards.sorted.map(_.literal).mkString("").hashCode
  }

  type Cards = List[Card]

  object Cards {
    def apply(string: String): Cards = {
      require(string.length % 2 == 0)
      val cards = string.grouped(2).toList.map(Card(_)).sorted(cardOrdering)
      // require(cards.forall(_.isValid))
      cards
    }
    def apply(hand: Int): Cards = {
      val x = 0.until(5).map(i => (hand >> (i * CARD_SHIFT)) & CARD_BITS)
      println(x)
      x.map(Card(_)).toList
    }

    //override def hashCode(): Int = this.sorted(cardOrdering.reverse)

    // Ac, Ad, Ah, As, Kc, ..., 2s
    val allCards: Cards = (for (rank <- Rank.allRanks; suit <- Suit.allSuits) yield Card(rank, suit)).sorted
  }

  implicit val cardsShow =
    Show.show[Cards](_.map(c => c.show).mkString(", "))

  def encode5Cards(cards: Cards): Int = encode5Cards(cards.map(_.toInt))
  def encode5Cards(cards: Seq[Int]): Int = {
    cards.sorted.foldLeft(0)((res, card) => (res << 6) + card)
  }

  trait CardsOps[A] {
    def literal: String
    def toInt: Int
  }

  implicit def cardsOps(cs: Cards): CardsOps[Cards] =
    new CardsOps[Cards] {
      def literal: String = cs.map(_.literal).mkString
      def toInt: Int = encode5Cards(cs.map(_.toInt).sorted(descOrdering))
    }

}
