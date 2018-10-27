package com.example.pokerlake.shared

import fastparse._
import NoWhitespace._
import com.example.pokerlake.shared.game.Rank.Ranks
import game.{Suited, _}
import Calc.choose


object Parser {

  def checkParse(x: Parsed[_]): Unit = {
    x match {
      case Parsed.Success(_, _)    => println("suxxess")
      case Parsed.Failure(_, _, _) => println("failute")
    }
  }

  def rank[_: P] = P(CharIn("AKQJT98765432")).!.map(s => Rank(s.head))
  def ranks[_: P] =
    P(rank.rep(min = 1, max = 4)).map(rs => rs.toList.asInstanceOf[Ranks])

  def doubleSuited[_: P] = P("d" ~ "s")
  def singleSuited[_: P] = P("s" ~ "s")
  def suitGiven[_: P] = P(doubleSuited | singleSuited)
  def suit[_: P] = suitGiven.?.!.map(s => Suited(s))

  def range[_: P] =
    // P(ranks ~ suit).map((t: (Seq[Rank], Suited)) => {
    P(ranks ~ suit).map((t: (Ranks, Suited)) => {
      Range(t._1, t._2)

    })
  checkParse(parse("AA", range(_)))

  def exclude[_: P] = P("!")
  def excludedRanks[_: P] = P(exclude ~ range)

  def countRange[_: P] = P(range ~ excludedRanks.rep ~ End).!

  val finalTest = "AAss ! AAA"
  val prepped2 = "AKJ9"

  val prepped = finalTest.replaceAll("\\s", "")

  // checkParse(parse(prepped, countRange(_)))

  val result = parse(prepped2, range(_))
  result match {
    case Parsed.Success(res, _)  => println(s"suxxess: ${res} ${res.count}")
    case Parsed.Failure(_, _, _) => println("failed to parse range")
  }
}
