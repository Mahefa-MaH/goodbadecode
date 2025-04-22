// Good Code: Using a for comprehension for concise and readable list processing.
val numbers = List(1, 2, 3, 4, 5)
val squares = for (num <- numbers) yield num * num

// Bad Code:  Unnecessary complexity and verbosity using recursion.
def calculateSquaresRecursive(list: List[Int]): List[Int] = list match {
  case Nil => Nil
  case head :: tail => head * head :: calculateSquaresRecursive(tail)
}
val squaresRecursive = calculateSquaresRecursive(numbers)


//Good Code: Using pattern matching for elegant type handling
sealed trait Result
case class Success(value: Int) extends Result
case class Failure(reason: String) extends Result

def processData(data: String): Result = {
  try {
    Success(data.toInt)
  } catch {
    case e: NumberFormatException => Failure("Invalid input")
  }
}

//Bad Code: Using exceptions for control flow.
def processDataBad(data:String): Int = {
  try{
    data.toInt
  } catch {
    case e: NumberFormatException => throw new RuntimeException("Invalid input")
  }
}

//Good Code: Using higher-order functions for flexible and reusable code.
val doubled = numbers.map(_ * 2)
val evens = numbers.filter(_ % 2 == 0)


//Bad Code:  Imperative style with mutable variables and loops.
var doubledImperative = List[Int]()
for(i <- 0 until numbers.length){
  doubledImperative = doubledImperative :+ numbers(i)*2
}

//Good Code: Using implicit parameters for dependency injection.
implicit val config = Config("mySetting")

class MyClass(implicit val config: Config) {
  def doSomething = println(config.setting)
}
case class Config(setting: String)

//Bad Code: Hardcoded dependencies.
class MyClassBad{
  val config = Config("hardcoded")
  def doSomething = println(config.setting)
}

//Good Code:  Leveraging type classes for polymorphism.
trait Show[A] {
  def show(a: A): String
}
object Show {
  implicit val intShow: Show[Int] = new Show[Int] {
    override def show(a: Int): String = a.toString
  }
  implicit val stringShow: Show[String] = new Show[String] {
    override def show(a: String): String = a
  }
}
def display[A](a:A)(implicit show:Show[A]): Unit = println(show.show(a))

//Bad code:  Using type casting and multiple if-else statements for handling different types.
def displayBad(a:Any):Unit={
  if(a.isInstanceOf[Int]){
    println(a.asInstanceOf[Int])
  } else if (a.isInstanceOf[String]){
    println(a.asInstanceOf[String])
  } else {
    println("Unsupported type")
  }
}

//Good Code: Using Actors for concurrency
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

val futureResult = Future {
  Thread.sleep(1000)
  10
}
val result = Await.result(futureResult, 2.seconds)



//Bad code: Shared mutable state for concurrency
var sharedCounter = 0
val threads = (1 to 10).map { _ =>
  new Thread {
    override def run(): Unit = {
      for (_ <- 1 to 10000) sharedCounter += 1
    }
  }
}
threads.foreach(_.start())
threads.foreach(_.join())


