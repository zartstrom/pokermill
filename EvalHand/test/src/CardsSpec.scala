
import com.example.pokerlake.shared.Const._
import org.scalatest._
import com.example.pokerlake.shared.PokerHand._
import com.example.pokerlake.shared.game._

class CardsSpec extends FlatSpec with Matchers {

  "Encode five cards and recreate cards" should "return the same cards" in {
    val cards = Cards("QsJd5h5d3h")

    Cards(encode5Cards(cards)) should be (cards)
  }
}