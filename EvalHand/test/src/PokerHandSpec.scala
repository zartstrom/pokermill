import org.scalatest._
import com.example.pokerlake.shared.PokerHand._
import com.example.pokerlake.shared.game._

class PokerHandSpec extends FlatSpec with Matchers {

  "A flush" should "be higher rated as straight" in {}

  "Straights" should "be ordered correctly" in {
    val s1 = Straight("KQJT9")
    val s2 = Straight("QJT98")

    assert(s1 > s2)
    assert(s1 == s1)
  }

  "Pokerhands" should "be comparable" in {
    val s1: PokerHand = Straight("KQJT9")
    val s2: PokerHand = Straight("QJT98")

    // val result = (s1 compare s2)
    assert(s1.compare(s2) == 1)

  }
  "A straight flush" should "be valued higher than quads" in {
    val straightFlush = StraightFlush(Club, Straight(List(King, Queen, Jack, Ten, Nine), King, Nine))
    val quads         = Quads(Ace, List(Ten))

    assert((straightFlush compare quads) == 1)
    assert((quads compare straightFlush) == -1)
  }

  "The Pokerhands" should "have the right order in strength" in {
    val straightFlush = StraightFlush(Club, Straight(List(King, Queen, Jack, Ten, Nine), King, Nine))
    val quads         = Quads(Ace, List(Ten))
    val fullHouse     = FullHouse(Ten, Eight)
    val flush         = Flush(Heart, List(Ace, Eight, Seven, Four, Three))
    val straight      = Straight(List(King, Queen, Jack, Ten, Nine), King, Nine)
    val threeOfAKind  = ThreeOfAKind(Queen, List(Ace, Deuce))
    val twoPair       = TwoPair(Queen, Jack, List(Deuce))
    val pair          = Pair(Queen, List(Nine, Four, Deuce))
    val highCard      = HighCard(King, List(Jack, Nine, Four, Deuce))

    assert((straightFlush compare quads) == 1)
    assert((quads compare fullHouse) == 1)
    assert((fullHouse compare flush) == 1)
    assert((flush compare straight) == 1)
    assert((straight compare threeOfAKind) == 1)
    assert((threeOfAKind compare twoPair) == 1)
    assert((twoPair compare pair) == 1)
    assert((pair compare highCard) == 1)

    assert((quads compare straightFlush) == -1)
    assert((fullHouse compare quads) == -1)
    assert((flush compare fullHouse) == -1)
    assert((straight compare flush) == -1)
    assert((threeOfAKind compare straight) == -1)
    assert((twoPair compare threeOfAKind) == -1)
    assert((pair compare twoPair) == -1)
    assert((highCard compare pair) == -1)
  }

  "A Fullhouse" should "rank higher than another Fullhouse if has a higher set rank" in {
    val fh1 = FullHouse(Ace, Jack)
    val fh2 = FullHouse(King, Queen)
    val fh3 = FullHouse(Ten, Eight)
    val fh4 = FullHouse(Eight, Ten)

    assert(fh1.handValue > fh2.handValue)
    assert(fh3.handValue > fh4.handValue)
    (fh1 compare fh2) should be(1)
    (fh2 compare fh1) should be(-1)
    (fh3 compare fh4) should be(1)
    (fh4 compare fh3) should be(-1)
  }

  it should "rank higher than another Fullhouse if the set rank equals but has higher pair" in {
    val fh5 = FullHouse(Seven, Five)
    val fh6 = FullHouse(Seven, Four)
    (fh5 compare fh6) should be(1)
    (fh6 compare fh5) should be(-1)
  }

  "A Highcard" should "rank higher if it is the higher card" in {
    val hc1 = HighCard(Ace, List(Queen, Ten, Eight, Deuce))
    val hc2 = HighCard(King, List(Queen, Ten, Eight, Deuce))
    (hc1 compare hc2) should be(1)
  }

  it should "rank higher if it has higher kickers" in {
    val hc3 = HighCard(King, List(Queen, Ten, Eight, Three))
    val hc4 = HighCard(King, List(Jack, Ten, Eight, Three))
    val hc5 = HighCard(King, List(Queen, Nine, Eight, Three))
    val hc6 = HighCard(King, List(Queen, Ten, Seven, Three))
    val hc7 = HighCard(King, List(Queen, Ten, Eight, Deuce))

    (hc3 compare hc4) should be(1)
    (hc3 compare hc5) should be(1)
    (hc3 compare hc6) should be(1)
    (hc3 compare hc7) should be(1)
  }
}
