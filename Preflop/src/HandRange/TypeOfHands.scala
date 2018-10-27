package HandRange

trait TypeOfHands {
  def count: Int
  def countDs: Int = count * 36 / 256
}
