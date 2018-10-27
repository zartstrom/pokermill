package HandRange

import com.example.pokerlake.shared.Calc.choose

object AcesHands extends TypeOfHands {

  // AA**

  def count: Int = choose(2, 4) * choose(2, 48)
}
