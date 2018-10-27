import org.scalatest._
import com.example.pokerlake.shared.Const._
import com.example.pokerlake.shared.FiveCards._
import com.example.pokerlake.shared.PokerHand._
import com.example.pokerlake.shared.game._

class EvalFiveCardsSpec extends FlatSpec with Matchers {

  "A card" should "be converted to byte correctly" in {
    val c1 = Card("9c")
    val c2 = Card("Qs")
    val c3 = Card("Ad")

    c1.toInt should be((Nine.highVal << RANK_SHIFT) + Club.suitNumber)
    c2.toInt should be((Queen.highVal << RANK_SHIFT) + Spade.suitNumber)
    c3.toInt should be((Ace.highVal << RANK_SHIFT) + Diamond.suitNumber)
  }

  "Ranks" should "be counted correctly" in {
    val cards1 = Cards("JhJsTd7c6c")
    val cards2 = Cards("JhJsTdTcTc")

    val descOrdering = Ordering[Int].reverse

    val cardInts1 = cards1.map(_.toInt).sorted(descOrdering)
    val cardInts2 = cards2.map(_.toInt).sorted(descOrdering)

    ranks(cardInts1) should be(64 * 2 + 16 + 4 + 1)
    ranks(cardInts2) should be(64 * 3 + 16 * 2)
  }

  "EvalFiveCards" should "classify a hand as straight flush" in {
    val cards = Cards("9c8c7c6c5c")
    val pokerHand = evalFiveCards(cards)

    pokerHand should matchPattern { case StraightFlush(_, _) => }
    pokerHand should have(
      'suit (Club),
      'straight (Straight(List(Nine, Eight, Seven, Six, Five), Nine, Five))
    )
  }

  it should "classify a hand as quads" in {
    val cards = Cards("KcTcTdThTs")
    val pokerHand = evalFiveCards(cards)

    pokerHand should matchPattern { case Quads(_, _) => }
    pokerHand should have(
      'rank (Ten),
      'kickers (List(King))
    )
  }

  it should "classify a hand as flush" in {
    val cards = Cards("KcQc9c6c5c")
    val pokerHand = evalFiveCards(cards)

    pokerHand should matchPattern { case Flush(_, _) => }
    pokerHand should have(
      'suit (Club),
      'ranks (List(King, Queen, Nine, Six, Five))
    )
  }

  it should "classify a hand as straight" in {
    val cards = Cards("KsQcJhTd9c")
    val pokerHand = evalFiveCards(cards)

    pokerHand should matchPattern { case Straight(_, _, _) => }
    pokerHand should have(
      'ranks (List(King, Queen, Jack, Ten, Nine)),
      'lowCard (Nine),
      'highCard (King)
    )
  }

  it should "classify a hand as three of a kind" in {
    val cards = Cards("QsTc4d4h4s")
    val pokerHand = evalFiveCards(cards)

    pokerHand should matchPattern { case ThreeOfAKind(_, _) => }
    pokerHand should have(
      'rank (Four),
      'kickers (List(Queen, Ten))
    )
  }

  it should "classify a hand as two pair" in {
    val cards = Cards("QsTcTd4h4s")
    val pokerHand = evalFiveCards(cards)

    pokerHand should matchPattern { case TwoPair(_, _, _) => }
    pokerHand should have(
      'highRank (Ten),
      'lowRank (Four),
      'kickers (List(Queen))
    )
  }

  it should "classify a hand as pair" in {
    val cards = Cards("QsTc4d4h3s")
    val pokerHand = evalFiveCards(cards)

    pokerHand should matchPattern { case Pair(_, _) => }
    pokerHand should have(
      'rank (Four),
      'kickers (List(Queen, Ten, Three))
    )
  }

  it should "classify a hand as high card" in {
    val cards = Cards("QsTc4d3h2s")
    val pokerHand = evalFiveCards(cards)

    pokerHand should matchPattern { case HighCard(_, _) => }
    pokerHand should have(
      'rank (Queen),
      'kickers (List(Ten, Four, Three, Deuce))
    )
  }
}
