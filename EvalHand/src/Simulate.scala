package com.example.pokerlake.shared

import com.example.pokerlake.shared.Const._
import com.example.pokerlake.shared.PokerHand.{FullHouse, PokerHand}
import Const.descOrdering
import com.example.pokerlake.shared.game._
import com.example.pokerlake.shared.FiveCards._

import scala.collection.mutable

object Simulate extends App {

  def bestHandHoldem(pocket: Cards, board: Cards): PokerHand = {
    require(pocket.size == 2)
    require(board.size == 5)

    val cardInts: Seq[Int] = (pocket ++ board).map(_.toInt).sorted(descOrdering)

    val (_, hand): (Int, Int) = cardInts.combinations(5).map(encode5Cards).map(x => (eval(x), x)).max

    println(hand)
    println(Cards(hand))

    evalFiveCards(Cards(hand))
  }

  def bestHandPLO(pocket: Cards, board: Cards): PokerHand = {
    require(pocket.size == 4)
    require(board.size == 5)

    val (_, hand) = (for {
      xx  <- pocket.combinations(2)
      yyy <- board.combinations(3)
    } yield {
      val encoded = encode5Cards((xx ++ yyy).map(_.toInt).sorted(descOrdering))
      (eval(encoded), encoded)
    }).max

    println(hand)
    println(Cards(hand))

    evalFiveCards(Cards(hand))
  }

  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I) = getOrElseUpdate(key, f(key))
  }

  def bestHandValue: ((Seq[Int], Seq[Int])) => Int = memoize { handAndBoard: (Seq[Int], Seq[Int]) =>
    (for {
      xx  <- handAndBoard._1.combinations(2)
      yyy <- handAndBoard._2.combinations(3)
    } yield {
      eval(encode5Cards((xx ++ yyy).map(_.toInt).sorted(descOrdering)))
    }).max
  }
  /*
  val pocket = Cards("QcTs")
  val board = Cards("JsTc9s8s7s")
  println(bestHandHoldem(pocket, board))
   */

  def simulate(hero: Cards, villain: Cards): Unit = {
    val hs = hero.map(_.toInt)
    val vs = villain.map(_.toInt)
    val bs = Cards.allCards.map(_.toInt).filter(x => !hs.contains(x) && !vs.contains(x))

    var win  = 0
    var tie  = 0
    var loss = 0

    bs.combinations(5).foreach { board =>
      val heroBest    = bestHandValue(hs, board)
      val villainBest = bestHandValue(vs, board)
      if (heroBest > villainBest) win += 1 else if (heroBest == villainBest) tie += 1 else loss += 1
    }
    println(win)
    println(tie)
    println(loss)
  }

  /*
  Algorithm Improved

  - evaluate for a hand (4 cards) all 3 card boards choose(3, 44)
    choose(2, 4) * choose(3, 44) * 2 calculations (~80k)

  - map 5 board cards to the choose(3, 5) = 10 three boards, get evaluation and do max
    choose(5, 44) then choose(3, 5) & max; two times  (~20mio)

   - compare resulting sequences for both hands

   What can we cache?
   - every five card hand evaluation
   - every hand and 3 card combi

   What can we preevaluate?
   - every five card hand evaluation (evaluation is an Int thats ok, how to serialize / encode the five cards)
   - all choose 3 of 52 combinations
   - boards to 10 three boards

   Useful knowhow:
   - how fast is it to have a Seq as key in a Map?!
   - how does scala serialize the keys of a Map? can we change the serialization mechanism?!
   - probably it is not serialize but hash :p

   */

  val pocketPLO = Cards("AdJhJs8s")
  val boardPLO  = Cards("9h6d4c5dAs")
  println(bestHandPLO(pocketPLO, boardPLO))

  val hero    = Cards("Ad7d6dKs")
  val villain = Cards("Qh5hQsTs")

  def time[T](block: => T): T = {
    val start     = System.currentTimeMillis
    val res       = block
    val totalTime = System.currentTimeMillis - start
    println("Elapsed time: %1d ms".format(totalTime))
    res
  }

  time { simulate(hero, villain) }
  time { simulate(hero, villain) }
  time { simulate(hero, villain) }
}
