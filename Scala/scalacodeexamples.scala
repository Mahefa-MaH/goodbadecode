// Good Code: Using pattern matching for concise and efficient handling of different cases.
def processData(data: Any): String = data match {
  case s: String => s.toUpperCase
  case i: Int => i.toString
  case l: List[_] => l.mkString(",")
  case _ => "Unknown data type"
}


// Bad Code: Overly complex and verbose solution using multiple if-else statements.
def processDataBad(data: Any): String = {
  if (data.isInstanceOf[String]) {
    data.asInstanceOf[String].toUpperCase
  } else if (data.isInstanceOf[Int]) {
    data.asInstanceOf[Int].toString
  } else if (data.isInstanceOf[List[_]]) {
    data.asInstanceOf[List[_]].mkString(",")
  } else {
    "Unknown data type"
  }
}


// Advanced Use Case:  Demonstrates how to use a monad transformer (EitherT) for error handling
import cats.data.EitherT
import cats.instances.either._
import cats.instances.future._
import cats.syntax.applicative._
import scala.concurrent.{ExecutionContext, Future}

//Good code example of EitherT
implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global

def fetchData(id: Int): EitherT[Future, String, Int] = {
  EitherT(Future {
    if (id > 0) Right(id * 2) else Left("Invalid ID")
  })
}

def processData(data: Int): EitherT[Future, String, String] = {
  EitherT(Future {
    if(data > 10) Right(data.toString) else Left("Data too small")
  })
}


def main(args:Array[String]):Unit = {
  fetchData(5).flatMap(processData).value.map(println)
  fetchData(-1).flatMap(processData).value.map(println)
}

//Bad Code Example of EitherT:  Excessive nesting and lack of composition
def fetchDataBad(id: Int): Future[Either[String, Int]] = Future {
  if (id > 0) Right(id * 2) else Left("Invalid ID")
}

def processDataBad(data: Int): Future[Either[String, String]] = Future {
  if (data > 10) Right(data.toString) else Left("Data too small")
}

def mainBad(args: Array[String]): Unit = {
  fetchDataBad(5).flatMap(result =>
    result match {
      case Right(data) => processDataBad(data).map(result2 => result2 match {
        case Right(finalResult) => println(finalResult)
        case Left(error) => println(error)
      })
      case Left(error) => Future {println(error)}
    }
  )
  fetchDataBad(-1).flatMap(result =>
    result match {
      case Right(data) => processDataBad(data).map(result2 => result2 match {
        case Right(finalResult) => println(finalResult)
        case Left(error) => println(error)
      })
      case Left(error) => Future {println(error)}
    }
  )
}

