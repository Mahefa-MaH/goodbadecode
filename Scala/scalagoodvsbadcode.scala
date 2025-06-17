// Good Code: Using immutable data structures and pattern matching for robust and readable code.
object GoodCode {
  sealed trait Result
  case class Success(value: Int) extends Result
  case class Failure(message: String) extends Result

  def processData(data: List[Int]): Result = {
    data match {
      case Nil => Failure("Empty data")
      case head :: tail if head < 0 => Failure("Negative value encountered")
      case head :: tail => Success(head * 2) //Example operation. Replace with desired operation.
    }
  }

  def main(args: Array[String]): Unit = {
    val goodData = List(1, 2, 3)
    val badData = List(-1, 2, 3)
    val emptyData = List()

    println(processData(goodData)) // Success(2)
    println(processData(badData))  // Failure(Negative value encountered)
    println(processData(emptyData)) // Failure(Empty data)

  }
}


// Bad Code: Mutable state and less readable structure.  Avoid this style.
object BadCode {
  def processData(data: Array[Int]): Int = {
    var result = 0
    var i = 0
    while (i < data.length) {
      if (data(i) < 0) {
        return -1 //Error Handling is poor
      }
      result += data(i) * 2 //Example operation. Replace with desired operation.
      i += 1
    }
    result
  }

  def main(args: Array[String]): Unit = {
    val goodData = Array(1, 2, 3)
    val badData = Array(-1, 2, 3)

    println(processData(goodData)) //6
    println(processData(badData))  //-1

  }
}
