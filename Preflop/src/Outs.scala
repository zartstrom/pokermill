import com.example.pokerlake.shared.Calc.choose

object Outs {

  def percFmt(x: Double): String = {
    "%.3f%%".format(x)
  }

  val result = 100.0 - 100.0 * choose(0, 4) * choose(3, 44) / choose(3, 48)

  println(percFmt(result))


}
