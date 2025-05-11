// Good Code: Using pattern matching for robust error handling and concise code.
def processData(data: Option[String]): Either[String, Int] = data match {
  case Some(s) => 
    try {
      Right(s.toInt)
    } catch {
      case e: NumberFormatException => Left(s"Invalid input: $s")
    }
  case None => Left("No data provided")
}


// Bad Code:  Repetitive error handling and less readable.
def processDataBad(data: Option[String]): Int = {
  if (data.isEmpty) {
    throw new IllegalArgumentException("No data provided")
  } else {
    try {
      data.get.toInt
    } catch {
      case e: NumberFormatException => {
        println("Invalid input")
        0 // or some arbitrary default value, which is error prone
      }
    }
  }
}


// Advanced use case showcasing monad transformers for complex error handling and optionality.
import cats.data.{EitherT, OptionT}
import cats.instances.either._
import cats.instances.option._
import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.functor._

def complexProcess(data: Option[String]): EitherT[Option, String, Int] = {
  OptionT(data).semiflatMap(s => 
    EitherT.fromEither[Option](
      try {
        s.toInt.asRight[String]
      } catch {
        case e: NumberFormatException => s"Invalid input: $s".asLeft[Int]
      }
    )
  )
}

