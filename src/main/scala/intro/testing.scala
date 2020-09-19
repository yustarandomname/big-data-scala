package intro

object testing {
  def onlyEvenNumbers(xs: List[Int]): List[Int] = xs match {
    case x if x.isEmpty => Nil

    // if head is odd
    case x if x.head % 2 == 1 =>  onlyEvenNumbers(xs.tail)

    // if head is even
    case x if x.head % 2 == 0 =>  x.head :: onlyEvenNumbers(xs.tail)


  }


  def main(args: Array[String]): Unit = {
    val xs = List(1,2,3,4,5,6,7)

    val res = onlyEvenNumbers(xs)


    println(res)
  }
}
