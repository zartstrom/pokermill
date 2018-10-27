package com.example.pokerlake.shared


import com.example.pokerlake.shared.PokerHand._
import com.example.pokerlake.shared.game.{Rank, Suit}

object Serde {

  def serialize(li: List[Int], ph: PokerHand): String = {
    s"""${li.mkString(",")};${ph.serialize}"""
  }

  def deserialize(string: String): (List[Int], PokerHand) = {
    val arr = string.split(";")
    require(arr.size == 2)
    val li: List[Int] = arr(0).split(",").map(_.toInt).toList

    val phData: Array[String] = arr(1).split(",")
    val abbreviation: String = phData(0)
    val info: String = phData(1)
    val ph = unpicklePokerHand(abbreviation, info)
    (li, ph)
  }

  def unpicklePokerHand(phStr: String, dataStr: String): PokerHand = {
    true match {
      case _ if phStr == StraightFlush.abbr => StraightFlush(Suit(dataStr.head), Straight(dataStr.tail.take(5)))
      case _ if phStr == Quads.abbr => Quads(Rank(dataStr.head), List(Rank(dataStr.tail.head)))
      case _ if phStr == FullHouse.abbr => FullHouse(Rank(dataStr.head), Rank(dataStr.tail.head))
      case _ if phStr == Flush.abbr => Flush(Suit(dataStr.head), dataStr.map(Rank(_)).toList)
      case _ if phStr == Straight.abbr => Straight(dataStr)
      case _ if phStr == ThreeOfAKind.abbr => ThreeOfAKind(Rank(dataStr.head), dataStr.tail.map(Rank(_)).toList)
      case _ if phStr == TwoPair.abbr => TwoPair(Rank(dataStr.head), Rank(dataStr.tail.head), dataStr.tail.tail.map(Rank(_)).toList)
      case _ if phStr == Pair.abbr => Pair(Rank(dataStr.head), dataStr.tail.map(Rank(_)).toList)
      case _ if phStr == HighCard.abbr => HighCard(Rank(dataStr.head), dataStr.tail.map(Rank(_)).toList)
    }
  }
}
