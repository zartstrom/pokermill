package HandRange

import com.example.pokerlake.shared.Calc.choose

object SuitedAceHands extends TypeOfHands {
  /*
Description:
Suited ace with a rundown, a pair, or two Broadway cards

Example:
A 9 8 6
A T T 3
A K Q 2

Strength component(s):
The "backbone" of this starting hand structure is the nut flush potential.
In addition, the side cards give us various possibilities.
   */

  def count: Int = countWithPair + countWithRundown + countTwoHighCards

  // with rundown
  val countWithRundown = 3 * 5 * 36
  // T98, T97, T87?!, T86?! till 654, 653, 643?!, 642?!
  // lets take three shapes (T98, T97, T87)
  // 5 ranks 6 to T
  // 37 means suited, see calculation below but maybe leave out quadruple suited hands
  // so take 36


  // with pair
  // pair not low please?!
  val countWithPair = 4 * 12 * 11 * (3 * 3 + choose(2, 3) + 3)
  // 12 ranks for pair, 11 ranks for other card
  // then assured doubled suitedness. Either
  // * a pair card has ace suit =>  3 * 3
  // * the single card has ace suit => choose(2, 3)
  // * or both  => 3

  // two high cards
  // choose 2 from T to K and a card 8 or lower
  // then double suitedness
  // * ace and one suit  3 ranks * 3 other suits * 3 other suits
  // * ace and two suits 3 ranks (the not suited) * 3 other suits
  // * ace and three suits 1 only one choice
  // Alternatively: no suit 3 * 3 * 3 from 4 * 4 * 4; 64 - 3 * 3 * 3 = 3 * 3 * 3 + 3 * 3 + 1
  val countTwoHighCards = (4 * choose(2, 4) * 7) * (3 * 3 * 3 + 3 * 3 + 1)

}
