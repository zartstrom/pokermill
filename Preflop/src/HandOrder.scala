package blub

import scala.io.Source
import com.example.pokerlake.shared.Calc.choose
import java.io._

import com.example.pokerlake.shared.game._

object HandOrder {

  val classToCount: Map[String, Int] = List("(kl)(kl)" -> choose(2, 4),
                                        "(kl)(km)" -> 4 * 3,
                                        "(kl)kl"   -> 4 * 3 * 2,
                                        "(kl)km"   -> 4 * 3 * 2,
                                        "(kl)(lm)" -> 4 * 3,
                                        "kk(lm)"   -> choose(2, 4) * 2,
                                        "(kl)(ml)" -> 4 * 3,
                                        "(klm)k"   -> 4 * 3,
                                        "kkll"     -> choose(2, 4),
                                        "kklm"     -> choose(2, 4) * 2,
                                        "(kl)lm"   -> 4 * 3 * 2,
                                        "k(lm)l"   -> 4 * 3 * 2,
                                        "(kl)mm"   -> 4 * choose(2, 3),
                                        "(kl)ml"   -> 4 * 3 * 2,
                                        "k(lm)m"   -> 4 * 3 * 2,
                                        "(klm)l"   -> 4 * 3,
                                        "(klm)m"   -> 4 * 3,
                                        "(kl)(mn)" -> 4 * 3,
                                        "kllm"     -> 4 * choose(2, 3) * 1,
                                        "klmm"     -> 4 * 3 * 1,
                                        "(kl)kk"   -> 4 * choose(2, 3),
                                        "(kl)mn"   -> 4 * 3 * 2,
                                        "k(lm)n"   -> 4 * 3 * 2,
                                        "(kl)ll"   -> 4 * choose(2, 3),
                                        "kl(mn)"   -> 4 * 3 * 2,
                                        "(klm)n"   -> 4 * 3,
                                        "k(lmn)"   -> 4 * 3,
                                        "kkkl"     -> choose(3, 4),
                                        "(klmn)"   -> 4,
                                        "klmn"     -> 4 * 3 * 2 * 1,
                                        "klll"     -> 4,
                                        "kkkk"     -> 1).toMap
  // TODO: maybe change the representation of a hand from string to something structured
  def noBrackets(string: String): String = {
    string.replace("(", "").replace(")", "")
  }
  def classify(hand: String): String = {
    val unsuited = hand.replace("(", "").replace(")", "")
    val ranks    = unsuited.distinct
    val map      = ranks.zip(List('k', 'l', 'm', 'n')).toMap
    val result   = map.foldLeft(hand)((hand, t) => hand.replace(t._1, t._2))
    result
  }
  def prevalence(hand: String): Int = classToCount.get(classify(hand)).get

  lazy val sortedHands = {
    val source               = Source.fromResource("oh3maxordering.txt")
    val strings: List[String] = source.getLines().toList
    source.close()

    strings.map(_.stripLineEnd).map(Hand(_))
  }

  case class HandRanked(hand: Hand, totalCount: Int)

  val handRanked: List[HandRanked] = sortedHands.foldLeft(List.empty[HandRanked], 0)((tup, hand) => {
    val ls = tup._1
    val count = tup._2
    val newCount = count + hand.frequency
    (HandRanked(hand, newCount) :: ls, newCount)
  })._1.reverse

  val nofHands = choose(4, 52)

  def top(f: Double): List[Hand] = {
    handRanked.filter(100.0 * _.totalCount / nofHands <= f).map(_.hand)
  }

  def topFilter(percent: Double, predicate: Hand => Boolean): List[Hand] = {
    val threshold = (percent / 100) * nofHands

    val tmp = handRanked.filter(x => predicate(x.hand))
    val handRs = tmp.foldLeft((List.empty[HandRanked], 0))((tup, handr) => {
      if (tup._2 < threshold) {
        (handr :: tup._1, handr.hand.frequency + tup._2)
      } else {
        (tup._1, handr.hand.frequency + tup._2)
      }
    })._1.reverse
    handRs.map(_.hand)

  }

  // def noAce: String => Boolean = x => !x.contains('A')
  def noAce(hand: Hand): Boolean = !hand.contains(Ace)
  def noAK(hand: Hand): Boolean = !hand.contains(Ace) && !hand.contains(King)
  // def noPair(hand: String);: Boolean = hand.replace
}
