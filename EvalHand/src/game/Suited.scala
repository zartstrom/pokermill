package com.example.pokerlake.shared.game

sealed trait Suited
case object DoubleSuited extends Suited
case object SingleSuited extends Suited
case object Single3Suited extends Suited
case object Single4Suited extends Suited
case object Rainbow extends Suited
case object AnySuited extends Suited
case object InvalidSuited extends Suited

object Suited {
  def apply(s: String): Suited = {
    if (s == "ds") DoubleSuited
    else if (s == "ss") SingleSuited
    else if (s == "") AnySuited
    else InvalidSuited
  }
}