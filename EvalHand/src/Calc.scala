package com.example.pokerlake.shared

import Const._
import PokerHand._

import scala.math.pow

object Calc {

  implicit class PowerInt(i: Int) {
    def **(b: Int): Int = pow(i, b).intValue
  }

  def choose(k: Int, n: Int): Int = {
    if (0 <= k && k <= n) {
      var nvar: Int = n
      var ntok: Int = 1
      var ktok: Int = 1
      1.to(math.min(k, n - k)).foreach({ t =>
        ntok *= nvar
        ktok *= t
        nvar -= 1
      })
      ntok / ktok
    } else 0
  }

  def percFmt(x: Float): String = {
    "%.3f%%".format(x)
  }

  val nofTotalFlops: Float = choose(3, 50).toFloat

  def percOfFlop(nofFlops: Int): Float = {
    100 * nofFlops / nofTotalFlops
  }

  def perc(street: Street)(combos: Int) = {
    val total = choose(street.nofCards, 50)
    percFmt(100 * combos.toFloat / total)
  }

  def perc = (percOfFlop _) andThen (percFmt _)

  case class Pocket(card1: Card, card2: Card)

  val hand65s = Pocket(Card("6d"), Card("5d"))

  val nofFlops: Float = choose(3, 50).toFloat

  // def suitFlops(nofSuits: Int, nofSuitPocket: Int = 2): Int = {
  def suitFlops(nofSuits: Int): Int = {
    val k = nofSuits
    val n = 13 - 2 // nofSuitPocket
    val nofCardsPocket = 2
    choose(k, n) * choose(3 - k, 52 - nofCardsPocket - n)
  }

  val res = 0.to(3).map((suitFlops _).andThen(percOfFlop _).andThen(percFmt _))
  // println("Suits: " + res.zip(0.to(3)))

  val street234 = choose(1, 4) * choose(1, 4) * choose(1, 4) * choose(0, 38)
  // println(perc(street234))
  val streets_ = 4 * choose(1, 4) * choose(1, 4) * choose(1, 4) * choose(0, 38)
  // println("Street: " + perc(streets_))

  val openEnder34 = choose(1, 4) * choose(1, 4) * choose(0, 4) * choose(0, 4) * choose(1, 34)
  // println(perc(openEnder34))
  val openEnder = 3 * openEnder34
  // println("Open Ender: " + perc(openEnder))


  case class Street(name: String, nofCards: Int)

  val flop = Street("Flop", 3)
  val turn = Street("Turn", 4)
  val river = Street("River", 5)

  val streets = List(flop, turn, river)

  case class Hand(name: String, nofCombos: Int)

  val rainbowFlopXXs = Hand("Rainbow Flop XXs", 3 * choose(1, 11) * choose(1, 13) * choose(1, 13) + choose(1, 13) ** 3)
  // println(rainbowFlopXXs.name + ": " + perc(flop)(rainbowFlopXXs.nofCombos))
  val rainbowFlopXXo = Hand("Rainbow Flop XXo", 2 * (choose(1, 12) ** 2) * choose(1, 13) + 2 * choose(1, 12) * (choose(1, 13) ** 2))
  // println(rainbowFlopXXo.name + ": " + perc(flop)(rainbowFlopXXo.nofCombos))

  case class RankHand(name: String, nofCombos: Int, nofCards: Int)

  val singlePair = RankHand("SinglePair", 2 * choose(1, 3) * choose(0, 3), 1)
  val twoPair = RankHand("TwoPair", choose(1, 3) * choose(1, 3), 2)
  val trips = RankHand("Trips", 2 * choose(2, 3) * choose(0, 3), 2)
  val fullHouse = RankHand("FullHouse", 2 * choose(1, 3) * choose(2, 3), 3)
  val quads = RankHand("Quads", 2 * choose(3, 3) * choose(0, 3), 3)

  val rankHands = List(singlePair, twoPair, trips, fullHouse, quads)

  def combos(hand: RankHand, street: Street): Int = {
    hand.nofCombos * choose(street.nofCards - hand.nofCards, 44)
  }

  /*
  println
  println(s"           | ${streets.map(s => "%7s".format(s.name)).mkString(" | ")}")
  rankHands.map(h => (h, streets.map(s => perc(s)(combos(h, s))))).foreach({ t =>
    val hand = t._1
    val handString = "%-10s".format(hand.name)
    val ps = t._2
    val psString = ps.map(p => "%7s".format(p)).mkString(" | ")
    println(s"${handString} | ${psString}")
  })
  */
}
