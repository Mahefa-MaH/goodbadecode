// Good Code: Using pattern matching for concise and efficient type handling.
def processData(data: Any): String = data match {
  case i: Int => s"Integer: $i"
  case s: String => s"String: $s"
  case l: List[_] => s"List: ${l.mkString(",")}"
  case _ => "Unknown data type"
}


// Bad Code: Excessive nesting and unclear logic.  Difficult to maintain and understand.
def processDataBad(data: Any): String = {
  if (data.isInstanceOf[Int]) {
    data.asInstanceOf[Int].toString()
  } else if (data.isInstanceOf[String]) {
    "String: " + data.toString()
  } else if (data.isInstanceOf[List[_]]) {
    val list = data.asInstanceOf[List[_]]
    "List: " + list.map(_.toString).mkString(",")
  } else {
    "Unknown data type"
  }
}

//Example usage
println(processData(10))
println(processData("hello"))
println(processData(List(1,2,3)))
println(processData(10.5))


println(processDataBad(10))
println(processDataBad("hello"))
println(processDataBad(List(1,2,3)))
println(processDataBad(10.5))
