package HandRange

import com.example.pokerlake.shared.Calc.choose

object General {

  lazy val totalNofHands: Int = choose(4, 52)

  def percOfTotalHands(nofHand: Int): Double = {
    100.0 * nofHand / totalNofHands
  }

  def percFmt(x: Double): String = {
    "%.3f%%".format(x)
  }

  // def percFmtOfTotalHands(nofHand: Int): String = percOfTotalHands _ andThen percFmt
  def percFmtOfTotalHands = percOfTotalHands _ andThen percFmt

}
