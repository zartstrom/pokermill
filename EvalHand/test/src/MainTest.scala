
/*
class EvalHandSpec extends FlatSpec with Matchers {

  "EvalHand" should "classify a hand as straight flush" in {
    val cards = Cards("9c8c7c6c5c4c3c")
    val pokerHand = eval(cards)

    pokerHand should matchPattern { case StraightFlush(_, _) => }
    pokerHand should have (
      'suit (Club),
      'straight (Straight(List(Five, Six, Seven, Eight, Nine), Nine, Five))
    )
  }

  "EvalHand" should "classify a hand as flush" in {
    val cards = Cards("KcQc9c6c5c")
    val pokerHand = eval(cards)

    pokerHand should matchPattern { case Flush(_, _) => }
    pokerHand should have (
      'suit (Club),
      'ranks (List(King, Queen, Nine, Six, Five))
    )
  }

  it should "classify a hand as pair" in {
    val cards = Cards("JhJsTd7c6c")
    val pokerHand = eval(cards)

    pokerHand should matchPattern { case Pair(_, _) => }
    pokerHand should have (
      'rank (Jack),
      'kickers (List(Ten, Seven, Six))
    )
  }

  "Straights" should "be ordered correctly" in {
    val s1 = Straight("KQJT9")
    val s2 = Straight("QJT98")

    assert(s1 > s2)
    assert(s1 == s1)
  }

  "Pokerhands" should "be comparable" in {
    val s1: PokerHand = Straight("KQJT9")
    val s2: PokerHand = Straight("QJT98")

    val result = handCompare(s1, s2)
    assert(result == 1)

  }
  "A straight flush" should "be valued higher than quads" in {
    val straightFlush = StraightFlush(Club, Straight(List(King, Queen, Jack, Ten, Nine), King, Nine))
    val quads = Quads(Ace, List(Ten))

    assert(handCompare(straightFlush, quads) == 1)
    assert(handCompare(quads, straightFlush) == -1)
  }


  "The Pokerhands" should "have the right order in strength" in {
    val straightFlush = StraightFlush(Club, Straight(List(King, Queen, Jack, Ten, Nine), King, Nine))
    val quads = Quads(Ace, List(Ten))
    val fullHouse = FullHouse(Ten, Eight)
    val flush = Flush(Heart, List(Ace, Eight, Seven, Four, Three))
    val straight = Straight(List(King, Queen, Jack, Ten, Nine), King, Nine)
    val threeOfAKind = ThreeOfAKind(Queen, List(Ace, Deuce))
    val twoPair = TwoPair(Queen, Jack, List(Deuce))
    val pair = Pair(Queen, List(Nine, Four, Deuce))
    val highCard = HighCard(King, List(Jack, Nine, Four, Deuce))

    assert(handCompare(straightFlush, quads) == 1)
    assert(handCompare(quads, fullHouse) == 1)
    assert(handCompare(fullHouse, flush) == 1)
    assert(handCompare(flush, straight) == 1)
    assert(handCompare(straight, threeOfAKind) == 1)
    assert(handCompare(threeOfAKind, twoPair) == 1)
    assert(handCompare(twoPair, pair) == 1)
    assert(handCompare(pair, highCard) == 1)

    assert(handCompare(quads, straightFlush) == -1)
    assert(handCompare(fullHouse, quads) == -1)
    assert(handCompare(flush, fullHouse) == -1)
    assert(handCompare(straight, flush) == -1)
    assert(handCompare(threeOfAKind, straight) == -1)
    assert(handCompare(twoPair, threeOfAKind) == -1)
    assert(handCompare(pair, twoPair) == -1)
    assert(handCompare(highCard, pair) == -1)
  }

  "A Fullhouse" should "rank higher than another Fullhouse if has a higher set rank" in {
    val fh1 = FullHouse(Ace, Jack)
    val fh2 = FullHouse(King, Queen)
    val fh3 = FullHouse(Ten, Eight)
    val fh4 = FullHouse(Eight, Ten)

    handCompare(fh1, fh2) should be(1)
    handCompare(fh2, fh1) should be(-1)
    handCompare(fh3, fh4) should be(1)
    handCompare(fh4, fh3) should be(-1)
  }

  it should "rank higher than another Fullhouse if the set rank equals but has higher pair" in {
    val fh5 = FullHouse(Seven, Five)
    val fh6 = FullHouse(Seven, Four)
    handCompare(fh5, fh6) should be (1)
    handCompare(fh6, fh5) should be (-1)
  }

  "Hashcode on Cards" should "work" in {
    val cards1 = Cards("JhJsTd7c6c")
    val cards2 = Cards("JsJhTd7c6c")

    // println(HashCards(cards1).hashCode)
    HashCards(cards1).hashCode should be (HashCards(cards2).hashCode)

    // too slooow
    // EvalHand.allHandsHashed.keys.size should be (2598960)  // choose 5 from 52
  }

  "AcAd versus KcKd" should "have the right evaluation" in {
    // Results to compare with come from PokerStove
    /*
    // too slooow
    val evaluation: Evaluation = playOut(Cards("AcAd"), Cards("KcKd"))

    evaluation.wins should be (1410336)
    evaluation.losses should be (292660)
    evaluation.ties should be (4664)
    */
  }
}
*/