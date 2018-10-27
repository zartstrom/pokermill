import blub.Hand
import blub.HandOrder.sortedHands

import org.scalatest._
import com.example.pokerlake.shared.PokerHand._
import com.example.pokerlake.shared.game._

class FrequencySpec extends FlatSpec with Matchers {

  "An unpaired hand" should "have frequency 24 if rainbow" in {
    val hand = Hand("T987")
    hand.frequency should be (24)
  }
  it should "have frequency 24 if single suited" in {
    val hand = Hand("(T9)87")
    hand.frequency should be (24)
  }
  it should "have frequency 12 if single 3 suited" in {
    val hand = Hand("(T98)7")
    hand.frequency should be (12)
  }
  it should "have frequency 4 if single 4 suited" in {
    val hand = Hand("(T987)")
    hand.frequency should be (4)
  }
  it should "have frequency 12 if double suited" in {
    val hand = Hand("(T9)(87)")
    hand.frequency should be (12)
  }

  "A single paired hand" should "have frequency 12 if rainbow" in {
    val hand = Hand("QQ64")
    hand.frequency should be (12)
  }
  it should "have frequency 24 if single suited and pair card suited" in {
    val hand = Hand("(Q6)Q4")
    hand.frequency should be (24)
  }
  it should "have frequency 12 if single suited and pair unsuited" in {
    val hand = Hand("QQ(64)")
    hand.frequency should be (12)
  }
  it should "have frequency 12 if single 3 suited" in {
    val hand = Hand("(Q64)Q")
    hand.frequency should be (12)
  }
  it should "have frequency 0 if single 4 suited" in {
    val hand = Hand("(QQ64)")
    hand.frequency should be (0)
  }
  it should "have frequency 12 if double suited" in {
    val hand = Hand("(Q6)(Q4)")
    hand.frequency should be (12)
  }

  "A single 3 paired hand" should "have frequency 4 if rainbow" in {
    val hand = Hand("7772")
    hand.frequency should be (4)
  }
  it should "have frequency 12 if single suited" in {
    val hand = Hand("(72)77")
    hand.frequency should be (12)
  }
  it should "have frequency 0 if single 3 suited" in {
    val hand = Hand("(772)7")
    hand.frequency should be (0)
  }
  it should "have frequency 0 if single 4 suited" in {
    val hand = Hand("(7772)")
    hand.frequency should be (0)
  }
  it should "have frequency 0 if double suited" in {
    val hand = Hand("(77)(72)")
    hand.frequency should be (0)
  }

  "A single 4 paired hand" should "have frequency 1 if rainbow" in {
    val hand = Hand("7777")
    hand.frequency should be (1)
  }
  it should "have frequency 0 if single suited" in {
    val hand = Hand("(77)77")
    hand.frequency should be (0)
  }
  it should "have frequency 0 if single 3 suited" in {
    val hand = Hand("(777)7")
    hand.frequency should be (0)
  }
  it should "have frequency 0 if single 4 suited" in {
    val hand = Hand("(7777)")
    hand.frequency should be (0)
  }
  it should "have frequency 0 if double suited" in {
    val hand = Hand("(77)(77)")
    hand.frequency should be (0)
  }

  "A double paired hand" should "have frequency 6 if rainbow" in {
    val hand = Hand("8866")
    hand.frequency should be (6)
  }
  it should "have frequency 24 if single suited" in {
    val hand = Hand("(86)86")
    hand.frequency should be (24)
  }
  it should "have frequency 0 if single 3 suited" in {
    val hand = Hand("(886)6")
    hand.frequency should be (0)
  }
  it should "have frequency 0 if single 4 suited" in {
    val hand = Hand("(8866)")
    hand.frequency should be (0)
  }
  it should "have frequency 6 if double suited" in {
    val hand = Hand("(86)(86)")
    hand.frequency should be (6)
  }

  "The sum of all frequencies" should "be the number of all possible starting hands" in {
    sortedHands.map(_.frequency).sum should be (270725)
  }
}

