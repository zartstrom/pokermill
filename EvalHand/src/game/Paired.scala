package com.example.pokerlake.shared.game

sealed trait Paired
case object Unpaired extends Paired
case object SinglePaired extends Paired
case object Single3Paired extends Paired
case object Single4Paired extends Paired
case object DoublePaired extends Paired

