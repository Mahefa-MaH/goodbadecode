// Good Code: Using immutable data structures and pattern matching for robust and efficient code.
object GoodCode {
  sealed trait Result[T]
  case class Success[T](value: T) extends Result[T]
  case class Failure(error: String) extends Result[Nothing]

  def processData(data: String): Result[Int] = {
    try {
      val num = data.toInt
      if (num > 0) Success(num) else Failure("Number must be positive")
    } catch {
      case e: NumberFormatException => Failure(s"Invalid input: ${e.getMessage}")
    }
  }

  def main(args: Array[String]): Unit = {
    processData("10").match {
      case Success(value) => println(s"Success: $value")
      case Failure(error) => println(s"Failure: $error")
    }
    processData("-5").match {
      case Success(value) => println(s"Success: $value")
      case Failure(error) => println(s"Failure: $error")
    }
    processData("abc").match {
      case Success(value) => println(s"Success: $value")
      case Failure(error) => println(s"Failure: $error")
    }
  }
}


// Bad Code:  Mutable variables, exception handling without specific error types, and unclear logic.
object BadCode {
  def processData(data: String): Int = {
    var result = 0
    try {
      result = data.toInt
      if (result < 0) throw new Exception("Negative number")
    } catch {
      case e: Exception => println("Error: " + e.getMessage)
    }
    result
  }

  def main(args: Array[String]): Unit = {
    println(processData("10"))
    println(processData("-5"))
    println(processData("abc"))
  }
}
