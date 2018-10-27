package com.example.pokerlake.shared.game

import Rank.Ranks
import com.example.pokerlake.shared.Calc.choose

case class Range(ranks: Ranks, suited: Suited) {
  def count: Int = {
    /* A range is given by 1-4 ranks (i.e. "AA") and a kind of suitedness (single suited, double suited, rainbow)

     */
    val nofFree: Int               = 4 - ranks.length
    val ranksCount: Map[Rank, Int] = ranks.groupBy(x => x).mapValues(_.size)

    val padded = ranksCount.values.toList.padTo(4, 0).toVector

    def h(i: Int): Int = if (i != 0) 4 else 0
    (for {
      a <- padded(0).to(if (padded(0) != 0) padded(0) + nofFree else 0)
      b <- padded(1).to(if (padded(1) != 0) padded(1) + nofFree + padded(0) - a else 0)
      c <- padded(2).to(if (padded(2) != 0) padded(2) + nofFree + padded(0) - a + padded(1) - b else 0)
      d <- padded(3).to(if (padded(3) != 0) padded(3) + nofFree + padded(0) - a + padded(1) - b + padded(2) - c else 0)
    } yield
      choose(a, h(padded(0))) *
        choose(b, h(padded(1))) *
        choose(c, h(padded(2))) *
        choose(d, h(padded(3))) *
        choose(4 - a - b - c - d, 52 - 4 * ranks.size)).sum
  }
}
