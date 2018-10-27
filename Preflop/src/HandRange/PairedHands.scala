package HandRange

import com.example.pokerlake.shared.Calc.choose

object PairedHands extends TypeOfHands {
  def name = "Paired Hands"
  /* hands with a high pair and other good stuff:
     - double paired
     - double suited
     - connectivity

     QQJT
   */

  def count: Int = doublePairedHands + broadwayPairedHands


  val doublePairedHands: Int = choose(2, 12) * choose(2, 4) * choose(2, 4)  // 66 * 36 = 2376
  // without AA
  // choose 2 from 12 ranks
  // choose 2 from 4 cards twice
  val doublePairedHandsDs: Int = doublePairedHands * 1 / 6
  val doublePairedHandsSs: Int = doublePairedHands * 4 / 6
  val doublePairedHandsRb: Int = doublePairedHands * 1 / 6
  // sure we want to leave out the bad ones like 3322 ?!
  // basic value: Flop a set (prob is 23,76% you flop a set with double paired hand)
  // connectedness and suitedness add value as ever

  val bw = 5  // broadway width 9, T, J, Q, K
  val broadwayPairedHands: Int = bw * choose(2, 4) * choose(2, bw - 1) * 4 * 4  // 30 * 96 = 2880
  // a pair from 9 to K and two other cards 9 to K
  // choose one from 9 to K
  val broadwayPairedHandsDs: Int = doublePairedHands * 6 * 2 / 96
  val broadwayPairedHandsSsPair: Int = doublePairedHands * 6 * 2 * 2 * 2 / 96 // single suited with one suit doubled, which is in pair
  val broadwayPairedHandsSsNonPair: Int = doublePairedHands * 6 * 2 / 96 // single suited with one suit doubled, which is not in pair
  val broadwayPairedHandsSs3: Int = doublePairedHands * 6 * 2  / 96  // one suit tripled
  val broadwayPairedHandsRb: Int = doublePairedHands * 6 * 2 / 96

  // there 96 different combinations choose(2, 4) * 4 * 4

  // also include a hand like JJT7 single suited


  // big and small
  // ie. KQ65 double suited

}
