package HandRange

import com.example.pokerlake.shared.Calc.choose


object BroadwayHands extends TypeOfHands {
  /* 4 cards T and higher, or 4 cards 9 and higher with an ace.

     Examples:
     K Q J T
     A Q J 9

     Strength component(s):
     High, connected cards that build high pairs, high two pair, high straights, and high flushes when suited.
   */

  def count: Int = {
    countTenPlus + countInclNine + beautifulThreeDs
  }
  def countTenPlus: Int = {
    // choose 4 of 5 ranks and build a hand
    choose(4, 5) * 4 * 4 * 4 * 4
  }
  def countInclNine: Int = {
    // take one ace and one 9, choose 2 of 4 ranks (T, J, Q, K) and build a hand
    4 * 4 * choose(2, 4) * 4 * 4
  }

  // beautiful three with a dangler
  // ie. KQJ2 double suited
  // KQJ, KQT, QJT and small card, lets say 7 or smaller
  val beautifulThreeDs = 3 * 4 * 4 * 4 * 24 * 9 / 64
  // 3 patterns, choose 4 cards, 9 / 64 take double suited only

}
