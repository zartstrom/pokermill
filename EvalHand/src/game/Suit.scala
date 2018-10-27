package com.example.pokerlake.shared.game

import cats.Show
import io.circe._
import io.circe.syntax._

trait Suit {
  def literal: String = "x"

  def symbol: String = "x"

  val suitNumber: Int
}

case object Club extends Suit {
  override def literal: String = "c"

  override def symbol = "♣"

  override val suitNumber = 3 // 8 // 1000 = 2 ** 3
}

case object Diamond extends Suit {
  override def literal: String = "d"

  override def symbol = "♦"

  override val suitNumber = 2 // 4 // 0100 = 2 ** 2
}

case object Heart extends Suit {
  override def literal: String = "h"

  override def symbol = "♥"

  override val suitNumber = 1 // 2 // 0010 = 2 ** 1
}

case object Spade extends Suit {
  override def literal: String = "s"

  override def symbol = "♠"

  override val suitNumber = 0 // 1 // 0001 = 2 ** 0
}

case object InvalidSuit extends Suit {
  override def literal: String = "?"

  override def symbol = "?"

  override val suitNumber = 4
}

object Suit {
  val intToSuit: Map[Int, Suit] =
    List(Club, Diamond, Heart, Spade).map(s => (s.suitNumber, s)).toMap
  val allSuits: List[Suit] = List(Club, Diamond, Heart, Spade)
  val charSuit: Map[Char, Suit] = allSuits.map(s => (s.literal.head, s)).toMap

  def apply(i: Int): Suit = intToSuit.getOrElse(i, InvalidSuit)

  implicit val suitOrdering: Ordering[Suit] =
    Ordering.by((s: Suit) => s.literal)
  implicit val suitShow = Show.show[Suit](_.symbol)

  implicit val encodeSuit: Encoder[Suit] = Encoder[Suit] {
    case s: Suit => s.literal.asJson
  }
  implicit val decodeSuit: Decoder[Suit] =
    Decoder[String].map(x => Suit(x.head))
}

