package blub

import HandRange._
import HandRange.General._
import HandOrder._
import com.example.pokerlake.shared.Calc.choose
import com.example.pokerlake.shared.game.{Ace, King, Rank}
import java.io._

object Presenter extends App {
  println(General.totalNofHands)

  val totalNofHands = BroadwayHands.count + RundownHands.count + PairedHands.count + AcesHands.count + SuitedAceHands.count

  /*
  println("My Range")
  println("Broadway Hands")
  println(percFmtOfTotalHands(BroadwayHands.count))
  // println(percFmtOfTotalHands(BroadwayHands.countDs))

  println("Rundown Hands")
  println(percFmtOfTotalHands(RundownHands.count))
  // println(percFmtOfTotalHands(RundownHands.countDs))

  println("Paired Hands")
  println(percFmtOfTotalHands(PairedHands.count))

  println("Aces Hands")
  println(percFmtOfTotalHands(AcesHands.count))
  println(AcesHands.count)

  println("Suited Ace Hands")
  println(percFmtOfTotalHands(SuitedAceHands.count))

  println("Total")
  println(percFmtOfTotalHands(totalNofHands))
  // println(totalNofHands)
  */
  println

  //for (line <- sortedHands.drop(500).take(10))
    //println(s"$line -> ${classify(line)}")
  val p: Double = 10.0
  // for (hand <- top(p)) println(hand)

  val myTop: Seq[Hand] = topFilter(p, {h => !h.containsExactly(Ace, 2) && !h.containsExactly(King, 2)})
  val nofTopHands = myTop.map(_.frequency).sum

  println(top(p).map(_.frequency).sum)
  println(top(p).filter(!_.contains(Ace)).map(_.frequency).sum)
  println(top(p).filter(_.containsExactly(Ace, 1)).map(_.frequency).sum)

  Rank.allRanks.reverse.foreach { r =>
    val rHands = myTop.filter(_.contains(r)).map(_.frequency).sum
    val perc = 100.0 * rHands / nofTopHands
    println(s"$r: ${percFmt(perc)}")
  }

 val my3betNoAce = topFilter(5.0, noAK)

  my3betNoAce.drop(my3betNoAce.size - 3).foreach(println)
  // println(my3betNoAce.size)
  val file = new File("/home/phil/files/topNonAceHands.txt")
  val bw = new BufferedWriter(new FileWriter(file))
  for (hand <- my3betNoAce) {
    bw.write(s"${hand}\n")
  }
  bw.close()


}
