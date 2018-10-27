import Ordering.Implicits._

object TestOrd {
  trait Fruit { def loveIt: Int }

  case class Apple(diameter: Int) extends Fruit { def loveIt: Int = 3000 }
  implicit val appleOrdering: Ordering[Apple] = Ordering.by { a => a.diameter }

  case class Pear(weight: Int) extends Fruit { def loveIt: Int = 2000 }
  implicit val pearOrdering: Ordering[Pear] = Ordering.by { a => a.weight }

  case class Banana(length: Int) extends Fruit { def loveIt: Int = 1000 }
  implicit val bananaOrdering: Ordering[Banana] = Ordering.by { a => a.length }

  // implicit val fruitOrdering: Ordering[Fruit] = Ordering.by { f => f.loveIt }
  implicit val fruitOrdering: Ordering[Fruit] = new Ordering[Fruit] {
    def compare(x: Fruit, y: Fruit): Int = {
      val cmpFruit = x.loveIt compare y.loveIt
      if (cmpFruit != 0) return cmpFruit
      require(x.getClass == y.getClass)
      (x, y) match {
        case (s: Apple, t: Apple)   => appleOrdering.compare(s, t)
        case (s: Pear, t: Pear)     => pearOrdering.compare(s, t)
        case (s: Banana, t: Banana) => bananaOrdering.compare(s, t)
        case (_, _) => 1
      }
    }
  }

  val apple1 = Apple(5)
  val apple2 = Apple(4)
  val pear = Pear(8)

  println(appleOrdering.compare(apple1, apple2))
  // println(apple1 compare apple2)  // compare is not a member of Apple
  println(apple1 > apple2)
  println(apple1 max apple2)

  println(apple1.asInstanceOf[Fruit] > pear)

  type Fruits = List[Fruit]

  val fruits1: Fruits = List(apple1, apple1)
  val fruits2: Fruits = List(apple2, apple2)

  println(fruits1 > fruits2)
  println(fruits1 equiv fruits2)

}
