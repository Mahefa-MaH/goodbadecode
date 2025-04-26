// Good Code: Using pattern matching for concise and efficient type handling
sealed trait Result[A]
case class Success[A](value: A) extends Result[A]
case class Failure(error: String) extends Result[Nothing]

def processData(data: String): Result[Int] = {
  try {
    val num = data.toInt
    Success(num * 2)
  } catch {
    case e: NumberFormatException => Failure(s"Invalid input: $e")
  }
}

val result = processData("123")
result match {
  case Success(value) => println(s"Processed successfully: $value")
  case Failure(error) => println(s"Processing failed: $error")
}


//Bad Code:  Unnecessary complexity and verbose error handling
def processDataBad(data:String):Int={
  var num:Int = 0
  try{
      num = Integer.parseInt(data)
      num * 2
  }catch{
      case e:NumberFormatException => {
          println("Invalid input, Returning default value 0")
          0
      }
      case ex:Exception => {
          println("An unexpected error occured")
          0
      }
  }
}
val resultBad = processDataBad("abc")
println(resultBad)

