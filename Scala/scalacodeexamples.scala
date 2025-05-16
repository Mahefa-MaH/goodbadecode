// Good Code: Using pattern matching for concise and efficient handling of different cases.
object GoodCode extends App {
  sealed trait Result
  case class Success(value: Int) extends Result
  case class Failure(reason: String) extends Result

  def processResult(result: Result): String = result match {
    case Success(value) => s"Success: $value"
    case Failure(reason) => s"Failure: $reason"
  }

  println(processResult(Success(10))) // Output: Success: 10
  println(processResult(Failure("Error"))) // Output: Failure: Error)
}


// Bad Code:  Using if-else if chain which is less readable and maintainable for multiple cases.
object BadCode extends App {
  sealed trait Result
  case class Success(value: Int) extends Result
  case class Failure(reason: String) extends Result

  def processResult(result: Result): String = {
    if (result.isInstanceOf[Success]) {
      val success = result.asInstanceOf[Success]
      s"Success: ${success.value}"
    } else if (result.isInstanceOf[Failure]) {
      val failure = result.asInstanceOf[Failure]
      s"Failure: ${failure.reason}"
    } else {
      "Unknown Result"
    }
  }

  println(processResult(Success(10))) // Output: Success: 10
  println(processResult(Failure("Error"))) // Output: Failure: Error
}

//Advanced Use Case:  Using implicits and type classes for flexible error handling and logging.

object AdvancedGoodCode extends App {
  trait Logger[T] {
    def log(t: T): Unit
  }

  implicit object ConsoleLogger extends Logger[String] {
    override def log(message: String): Unit = println(message)
  }

  sealed trait Result[T]
  case class Success[T](value: T) extends Result[T]
  case class Failure[T](reason: String) extends Result[T]

  def processResult[T](result: Result[T])(implicit logger: Logger[String]): Unit = result match {
    case Success(value) => logger.log(s"Success: $value")
    case Failure(reason) => logger.log(s"Failure: $reason")
  }

  processResult(Success(10))
  processResult(Failure("Something went wrong"))
}

