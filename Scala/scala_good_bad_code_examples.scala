// Good Code: Using pattern matching for concise and efficient handling of different cases.
object GoodCode extends App {
  sealed trait Result
  case class Success(value: Int) extends Result
  case class Failure(reason: String) extends Result

  def processResult(result: Result): String = result match {
    case Success(value) => s"Success: $value"
    case Failure(reason) => s"Failure: $reason"
  }

  println(processResult(Success(42)))      // Output: Success: 42
  println(processResult(Failure("Error"))) // Output: Failure: Error)
}


// Bad Code: Using if-else for the same task, less efficient and less readable.
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
  println(processResult(Success(42)))
  println(processResult(Failure("Error")))
}


// Advanced Good Code: Utilizing Monads for error handling and composition.

import cats.implicits._
import cats.data.EitherT

object AdvancedGoodCode extends App{
  type Result[A] = EitherT[Option, String, A]

  def operation1(x: Int): Result[Int] = EitherT.fromEither(Right(x * 2))
  def operation2(x: Int): Result[Int] = 
    if(x > 10) EitherT.rightT(x + 5) else EitherT.leftT("Value too small")
  def operation3(x: Int): Result[String] = EitherT.fromEither(Right(s"Result: $x"))


  val result: Result[String] = for {
    a <- operation1(5)
    b <- operation2(a)
    c <- operation3(b)
  } yield c
  println(result.value)
}

