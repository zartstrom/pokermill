package blub

import com.example.pokerlake.shared.game.Rank.Ranks
import com.example.pokerlake.shared.game._

import fastparse._
import NoWhitespace._

case class Hand(sameSuited: Seq[Ranks]) {
  lazy val ranks: Ranks = sameSuited.foldLeft(List.empty[Rank])(_ ++ _)
  lazy val suited: Suited = true match {
    case _ if sameSuited.size == 1                              => Single4Suited
    case _ if sameSuited.size == 2 && sameSuited.head.size == 2 => DoubleSuited
    case _ if sameSuited.size == 2                              => Single3Suited
    case _ if sameSuited.size == 3                              => SingleSuited
    case _ if sameSuited.size == 4                              => Rainbow
  }
  private lazy val ranksCount: Map[Rank, Int] = ranks.groupBy(x => x).mapValues(_.size)

  lazy val paired: Paired = true match {
    case _ if ranksCount.values.count(_ == 1) == 4 => Unpaired
    case _ if ranksCount.values.count(_ == 2) == 1 => SinglePaired
    case _ if ranksCount.values.count(_ == 3) == 1 => Single3Paired
    case _ if ranksCount.values.count(_ == 4) == 1 => Single4Paired
    case _ if ranksCount.values.count(_ == 2) == 2 => DoublePaired
  }
  lazy val hasPair: Boolean = paired != Unpaired

  // KcKdJhTh
  // lazy val pairWith2Suits: Boolean = singlePaired && suited == SingleSuited && stuff

  def frequency: Int = {
    (paired, suited) match {
      case (Unpaired, Rainbow)       => 4 * 3 * 2 * 1
      case (Unpaired, SingleSuited)  => 4 * 3 * 2
      case (Unpaired, Single3Suited) => 4 * 3
      case (Unpaired, Single4Suited) => 4
      case (Unpaired, DoubleSuited)  => 4 * 3
      case (SinglePaired, Rainbow)   => 6 * 2 * 1
      // KK(JT) => 6 * 2; (KJ)KT => 4 * 3 * 2
      case (SinglePaired, SingleSuited) =>
        if (sameSuited.filter(_.size == 1).flatten.toSet.size == 1) 12 else 24
      case (SinglePaired, Single3Suited)  => 4 * 3
      case (SinglePaired, Single4Suited)  => 0
      case (SinglePaired, DoubleSuited)   => 12
      case (DoublePaired, Rainbow)        => 6
      case (DoublePaired, SingleSuited)   => 6 * 2 * 2
      case (DoublePaired, Single3Suited)  => 0
      case (DoublePaired, Single4Suited)  => 0
      case (DoublePaired, DoubleSuited)   => 6
      case (Single3Paired, Rainbow)       => 4 * 1
      case (Single3Paired, SingleSuited)  => 4 * 3
      case (Single3Paired, Single3Suited) => 0
      case (Single3Paired, Single4Suited) => 0
      case (Single3Paired, DoubleSuited)  => 0
      case (Single4Paired, Rainbow)       => 1
      case (Single4Paired, SingleSuited)  => 0
      case (Single4Paired, Single3Suited) => 0
      case (Single4Paired, Single4Suited) => 0
      case (Single4Paired, DoubleSuited)  => 0
      case (_, AnySuited)                 => 0
      case (_, InvalidSuited)             => 0
    }
  }

  def contains(rank: Rank): Boolean                = ranks.contains(rank)
  def containsExactly(rank: Rank, n: Int): Boolean = ranks.count(_ == rank) == n

  override def toString: String = {
    // def addString(ranks: Ranks, string: String): String = ranks.map(_.literal).map(x => x + string).foldLeft("")(_ + _)
    // val cards: String = sameSuited.zip(List("x", "y", "z", "w")).map(t => addString(t._1, t._2)).foldLeft("")(_ + _)
    val cards: String =
      sameSuited.map(rs => if (rs.size > 1) s"(${rs.map(_.literal).mkString})" else rs.head.literal).mkString

    s"$cards, $frequency"
    // .map(t: [(Ranks, String)] => t._1.map((r: Rank) => r.literal + s))
  }
}

object Hand {

  def rank[_: P]: P[Rank]      = P(CharIn("AKQJT98765432")).!.map(s => Rank(s.head))
  def ranks[_: P]: P[Ranks]    = P(rank.rep(min = 1, max = 4)).map(rs => rs.toList)
  def multiple[_: P]: P[Ranks] = P("(" ~/ ranks ~ ")")
  def single[_: P]: P[Ranks]   = rank.!.map(s => List(Rank(s.head)))
  def handParse[_: P]: P[Hand] = (Start ~ (multiple | single).rep ~ End).map(seqRanks => Hand(seqRanks))

  def apply(string: String): Hand = {
    val x: Parsed[Hand] = parse(string, handParse(_))
    x.get.value // there is no bad input ;)

    x match {
      case Parsed.Success(res, _)  => res
      case Parsed.Failure(x, y, z) => println(x, y, z); Hand(List(List(Ace, King, Queen, Jack)))
    }
  }

}
