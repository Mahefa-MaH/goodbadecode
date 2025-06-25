// Good Code: Using immutable data structures and pattern matching for a concise and efficient solution.
object GoodCode extends App {
  sealed trait Result
  case class Success(value: Int) extends Result
  case class Failure(reason: String) extends Result

  def processData(data: List[Int]): Result = {
    data match {
      case Nil => Failure("Empty data")
      case head :: tail if head < 0 => Failure("Negative value encountered")
      case head :: tail => Success(head * tail.sum)
      
    }
  }

  val goodData = List(1, 2, 3)
  val badData = List(-1, 2, 3)
  val emptyData = List.empty[Int]

  println(processData(goodData))  //Success(6)
  println(processData(badData))   //Failure(Negative value encountered)
  println(processData(emptyData)) //Failure(Empty data)
}


// Bad Code: Mutable variables, unnecessary complexity, and poor error handling.
object BadCode extends App {
  def processData(data: List[Int]): Int = {
    var result = 0
    var error = false
    for (i <- data) {
      if (i < 0) {
        error = true
      } else {
        result += i
      }
    }
    if (error) {
      throw new Exception("Negative value encountered") //Poor error handling;  should use Option or Either.
    } else {
      result
    }
  }
  try{
    val goodData = List(1,2,3)
    val badData = List(-1,2,3)
    println(processData(goodData)) //6
    println(processData(badData))  //Exception
  }
  catch{
    case e:Exception => println(e.getMessage)
  }

}
