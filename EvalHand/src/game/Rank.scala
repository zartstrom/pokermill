package com.example.pokerlake.shared.game

import cats.Show
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._

trait Rank extends Ordered[Rank] {
  def literal: String = "X"
  val highVal: Int = 0

  def compare(that: Rank) = this.highVal compare that.highVal

}

case object Ace extends Rank {
  override def literal: String = "A"

  override val highVal = 14
}

case object King extends Rank {
  override def literal: String = "K"

  override val highVal = 13
}

case object Queen extends Rank {
  override def literal: String = "Q"

  override val highVal = 12
}

case object Jack extends Rank {
  override def literal: String = "J"

  override val highVal = 11
}

case object Ten extends Rank {
  override def literal: String = "T"

  override val highVal = 10
}

case object Nine extends Rank {
  override def literal: String = "9"

  override val highVal = 9
}

case object Eight extends Rank {
  override def literal: String = "8"

  override val highVal = 8
}

case object Seven extends Rank {
  override def literal: String = "7"

  override val highVal = 7
}

case object Six extends Rank {
  override def literal: String = "6"

  override val highVal = 6
}

case object Five extends Rank {
  override def literal: String = "5"

  override val highVal = 5
}

case object Four extends Rank {
  override def literal: String = "4"

  override val highVal = 4
}

case object Three extends Rank {
  override def literal: String = "3"

  override val highVal = 3
}

case object Deuce extends Rank {
  override def literal: String = "2"

  override val highVal = 2
}

case object AnyRank extends Rank {
  override def literal: String = "x"

  override val highVal = 0
}

case object InvalidRank extends Rank {
  override def toString: String = "?"

  override val highVal = -1
}

object Rank {
  val allRanks: List[Rank] = List(Deuce,
                                  Three,
                                  Four,
                                  Five,
                                  Six,
                                  Seven,
                                  Eight,
                                  Nine,
                                  Ten,
                                  Jack,
                                  Queen,
                                  King,
                                  Ace)
  implicit val rankShow = Show.show[Rank](_.literal)
  val charRank: Map[Char, Rank] = allRanks.map(r => (r.literal.head, r)).toMap
  val intToRank: Map[Int, Rank] = allRanks.map(r => (r.highVal, r)).toMap

  def apply(c: Char): Rank = charRank.getOrElse(c, InvalidRank)
  def apply(i: Int): Rank = intToRank.getOrElse(i, InvalidRank)

  implicit val encodeRank: Encoder[Rank] = Encoder[Rank] {
    case r: Rank => r.highVal.asJson
  }
  implicit val decodeRank: Decoder[Rank] = Decoder[Int].map(x => Rank(x))

  type Ranks = List[Rank]

  object Ranks extends Ordered[Ranks] {
    def compare(that: Ranks) = this compare that
    implicit val ranksShow =
      Show.show[Ranks](_.map((c: Rank) => c.toString).mkString(", "))
    implicit val decodeRanks: Decoder[Ranks] = Decoder[List[Rank]]
    /*
    {
      final def apply(as: Json): Rank.Ranks =
        as.asArray.get.map(x => Rank(x.asNumber.get.toInt.get)).toList
    }*/

    implicit val encodeRanks: Encoder[Ranks] = Encoder[Ranks] { rs: Ranks =>
      rs.asJson
    }

  }

  trait RanksOps[B] {
    // def desc(implicit elem2ordered: B => Ordered[B]): B
    def desc: B
    def value: Int
  }

  implicit def descRanks(r: Ranks): RanksOps[Ranks] =
    new RanksOps[Ranks] {
      def desc: Ranks = r.sorted.reverse
      def value: Int = {
        r.map(_.highVal).foldLeft(0)((res, highVal) => (res << 2) + highVal)
      }
    }
}
